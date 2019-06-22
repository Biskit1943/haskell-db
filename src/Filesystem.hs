module Filesystem
    ( ls
    , lsItem
    , cd
    , bd
    , pwd
    , fsRename
    , mkdir
    , rmdir
    , rm
    , cat
    , write
    , getRoot
    , FSZipper
    , FSItem(File, Folder)
    , getInitialState
    , sortFSItem
    )
where

import           Data.List                      ( break
                                                , sortOn
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Ord                       ( comparing )


{-
An Item in the Filesystem consist either of a File with a Name and Data in it or
a Folder which consists of FSItems which can be either File or Folder and so on.
-}
type Name = String
type Data = String
type Path = String
separator :: String
separator = "/"
data FSItem = File Name Data
    | Folder Name [FSItem]
    deriving (Show)

{-
The FSCrumb consists of the parent folder name (Name), the Items before the
currently focused Item and the Items after the currently focused Item.

The FSZipper represents the currently focused Item and the FSCrumb which has the
data about what is before and after that Item.
-}
data FSCrumb = FSCrumb Name [FSItem] [FSItem]
    deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

{-
This little fuction let us apply a function to something by writing x -: f
instead of f x
(Just for better readability on ghci)
-}
x -: f = f x

debug :: FSZipper -> IO ()
debug (Folder fname items, []) = do
    putStrLn "name:"
    print fname
    putStrLn "items:"
    mapM_ print items
debug (item, FSCrumb name ls rs : bs) = do
    putStrLn "name:"
    print name
    putStrLn "ls:"
    mapM_ print ls
    putStrLn "item:"
    print item
    putStrLn "rs:"
    mapM_ print rs
    putStrLn "bs:"
    mapM_ print bs

{-
Lists all contents of a directory
-}
ls :: FSZipper -> [Name]
ls (Folder name items, _) = lsH items
ls (File fname dat, FSCrumb name ls rs : bs) =
    lsH $ ls ++ [File fname dat] ++ rs

{-
This function is a helper for ls.
It collects the name from a FSItem and returns a list of them
-}
lsH :: [FSItem] -> [Name]
lsH []                      = []
lsH [Folder        name _ ] = [name]
lsH [File          name _ ] = [name]
lsH (Folder name _ :    xs) = name : lsH xs
lsH (File   name _ :    xs) = name : lsH xs

{-
Instead of returning just the Names, this function will list the FSItems of the
current Folder.
-}
lsItem :: FSZipper -> [FSItem]
lsItem (Folder name []   , _) = []
lsItem (Folder name items, _) = items
lsItem (File fname dat, FSCrumb cname ls rs : bs) =
    ls ++ [File fname dat] ++ rs

sortFSItem :: [FSItem] -> [FSItem]
sortFSItem []    = []
sortFSItem items = sortOn nameH items

nameH (Folder name _) = name
nameH (File   name _) = name

{-
Iterates over a given Path and returns the corresponding Folder as focus.
-}
cd :: Path -> FSZipper -> Either String FSZipper
cd ('/' : path) state                     = cd path $ getRoot state
cd path         (Folder fname fitems, bs) = case rs of
    "" -> case cdH ls state of
        Left  s  -> Left s
        Right ns -> Right ns
    "/" -> case cdH ls state of
        Left  s  -> Left s
        Right ns -> Right ns
    (sep : rrs) -> case cdH ls state of
        Left  s  -> Left s
        Right ns -> cd rrs ns
  where
    (ls, rs) = break ('/' ==) path
    state    = (Folder fname fitems, bs)
cd path (File fname dat, FSCrumb cname ls rs : bs) =
    cd path (Folder cname (ls ++ [File fname dat] ++ rs), bs)

{-
Set a directory as new focus.

If a folder is given the left items plus the item were at and the right items
are checked if the given name matches a Folder in this list. If a matching
Folder was found the Folder will be returned with Just(a) else Nothing is
returned.
If a File was given, the Folder in which this File is, is invoked on cd.
-}
cdH :: Name -> FSZipper -> Either String FSZipper
cdH name (Folder fname items, bs) = case rs of
    []             -> Left "no such directory found"
    (item : ritem) -> Right (item, FSCrumb fname ls ritem : bs)
    where (ls, rs) = break (folderNameIn name) items
cdH name (File fname dat, FSCrumb cname ls rs : bs) =
    cdH name (Folder cname (ls ++ [File fname dat] ++ rs), bs)

{-
bds as long as we are not in the root
-}
getRoot :: FSZipper -> FSZipper
getRoot (focus, []) = (focus, [])
getRoot state       = getRoot $ bd state

{-
cds one directory up. If a file is currently focused the folder of that file
will be invoked. If we are on Top of the FS the Top will be returned.
-}
bd :: FSZipper -> FSZipper
bd (Folder fname items, []) = (Folder fname items, [])
bd (Folder fname items, FSCrumb cname ls rs : bs) =
    (Folder cname (ls ++ [Folder fname items] ++ rs), bs)
bd (File fname dat, FSCrumb cname ls rs : bs) =
    bd (Folder cname (ls ++ [File fname dat] ++ rs), bs)

{-
Lists the path to the current Focused directory (if the focus is on a file, the
path to is parent directory is listed)
-}
pwd :: FSZipper -> Path
pwd (Folder fname _, []) = fname
pwd (Folder fname items, FSCrumb cname ls rs : bs) =
    pwd (Folder cname (ls ++ [Folder fname items] ++ rs), bs)
        ++ fname
        ++ separator
pwd (File fname dat, FSCrumb cname ls rs : bs) =
    pwd (Folder cname (ls ++ [File fname dat] ++ rs), bs)

{-
This will check if the given Name is the name of the given FSItem. But only
Folders are matched, a File will always return False
-}
folderNameIn :: Name -> FSItem -> Bool
folderNameIn name (Folder fname _) = fname == name
folderNameIn name (File   _     _) = False

{-
This function will search in the Items of FSZipper after a file named <Name> and
return a FSZipper which has this Item focused.

break: This function breaks a list (items) into two lists, where the first list
is a list of items for which the given function (nameIn) returned False, the
second list consist of the first item which returned True and everything which
came afterwards.

This item is then split into each of its elements (<the left side of the Item>
<the matched Item> <the right side of the Item>) and from that a FSCrumb is
created which is also returned.
-}
focusFile :: Name -> FSZipper -> Either String FSZipper
focusFile name (Folder fname items, bs) = case rs of
    []             -> Left "no such file"
    (file : items) -> Right (file, FSCrumb fname ls items : bs)
    where (ls, rs) = break (fileNameIn name) items
focusFile name (File fname dat, FSCrumb cname ls rs : bs) =
    focusFile name (Folder cname (ls ++ [File fname dat] ++ rs), bs)

{-
This will check if the given Name is the name of a given FSItem. But only Files
are matched, a Folder will always return False.
-}
fileNameIn :: Name -> FSItem -> Bool
fileNameIn name (Folder folderName _) = False
fileNameIn name (File   fileName   _) = name == fileName

{-
This function takes a Name, which is the FSItem name which will be renamed,
a Name which will be the new Name of that FSItem and the FSZipper context.

It will either return Nothing, if the file was not found or the new FSZipper
context.
-}
fsRename :: Name -> Name -> FSZipper -> Either String FSZipper
fsRename oldName newName (Folder fname items, bs) = case file of
    Right f -> Right $ f -: rename newName -: bd       -- Return the "old" focus not the file focus
    Left  _ -> case directory of
        Right d -> Right $ d -: rename newName -: bd   -- Return the "old" focus not the folder one
        Left  _ -> Left "no such file or directory"      -- Neither a file nor a folder matched
        where directory = cd oldName (Folder fname items, bs)   -- Focus directory
    where file = focusFile oldName (Folder fname items, bs)     -- Focus file
fsRename oldName newName (File fname dat, FSCrumb cname ls rs : bs) =
    fsRename oldName newName (Folder cname (ls ++ [File fname dat] ++ rs), bs)

-- Renames the currently focused FSItem and returns the new FSZipper focus
rename :: Name -> FSZipper -> FSZipper
rename newName (Folder name items, bs) = (Folder newName items, bs)
rename newName (File   name dat  , bs) = (File newName dat, bs)

{-
Create a Folder in the current directory (or the parent directory of the current
file)
-}
mkdir :: Name -> FSZipper -> Either String FSZipper
mkdir newName (Folder fname items, bs) = case rs of
    [] -> Right (Folder fname (Folder newName [] : items), bs)
    _  -> Left $ "directory " ++ newName ++ " already exits"
    where (ls, rs) = break (folderNameIn newName) items
mkdir newName (File fname dat, FSCrumb cname ls rs : bs) =
    mkdir newName (Folder cname (ls ++ [File fname dat] ++ rs), bs)

rmdir :: Name -> FSZipper -> Either String FSZipper
rmdir names state = rmdirs (splitOnWhitespace names) state

rmdirs :: [Name] -> FSZipper -> Either String FSZipper
rmdirs []       state = Right state
rmdirs (x : xs) state = case rmdirH x state of
    Left  s  -> Left s
    Right ns -> rmdirs xs ns

{-
Returns a list of Strings splitted by ' '. '\\ ' is kept as ' '
-}
splitOnWhitespace :: String -> [String]
splitOnWhitespace s = concatOnBackslash $ splitOn " " s

{-
concats Strings on '\\' with a whitespace
-}
concatOnBackslash :: [String] -> [String]
concatOnBackslash []              = []
concatOnBackslash [x]             = [x]
concatOnBackslash [[]       , xs] = []
concatOnBackslash [x@(_ : _), xs] = case last x of
    '\\' -> [(init x) ++ " " ++ xs]
    _    -> [x, xs]
concatOnBackslash ([]        : xx : xs) = []
concatOnBackslash (x@(_ : _) : xx : xs) = case last x of
    '\\' -> concatOnBackslash (((init x) ++ " " ++ xx) : xs)
    _    -> x : concatOnBackslash (xx : xs)

{-
Delets a Folder, but only when its emtpy. If it failed a message will be
provided in the Left side.
-}
rmdirH :: Name -> FSZipper -> Either String FSZipper
rmdirH name (Folder fname items, bs) = case rs of
    []                              -> Left $ "no such directory: " ++ name
    (Folder rmName rmItems : items) -> case rmItems of
        [] -> Right (Folder fname (ls ++ items), bs)
        _  -> Left $ "directory " ++ name ++ " is not empty"
    where (ls, rs) = break (folderNameIn name) items
rmdirH name (File fname dat, FSCrumb cname ls rs : bs) =
    rmdirH name (Folder cname (ls ++ [File fname dat] ++ rs), bs)

{-
Deletes one or more Files, splitted by whitespaces which are not escaped
-}
rm :: Name -> FSZipper -> Either String FSZipper
rm names state = rms (splitOnWhitespace names) state

rms :: [Name] -> FSZipper -> Either String FSZipper
rms []       state = Right state
rms (x : xs) state = case rmH x state of
    Left  s  -> Left s
    Right ns -> rms xs ns

{-
Removes a file from the current Focused Folder (or parent Folder of the focused
File).
-}
rmH :: Name -> FSZipper -> Either String FSZipper
rmH name (Folder fname items, bs) = case rs of
    []                        -> Left $ "no such file: " ++ name
    (File rmName dat : items) -> Right (Folder fname (ls ++ items), bs)
    where (ls, rs) = break (fileNameIn name) items
rmH name (File fname dat, FSCrumb cname ls rs : bs) =
    rmH name (Folder cname (ls ++ [File fname dat] ++ rs), bs)

cat :: Name -> FSZipper -> Either String (Data, FSZipper)
cat names state = case length namesl of
    0 -> Left "no files provided"
    1 -> case catH (head namesl) state of
        Left  s  -> Left s
        Right nd -> Right (nd, state)
    _ -> cats namesl "" state
    where namesl = splitOnWhitespace names

{-
Takes a list of names and concatenate the contens of all but the last into
the last and returns the new content and new state.
-}
cats :: [Name] -> Data -> FSZipper -> Either String (Data, FSZipper)
cats []  _   _     = Left "error: no names provided"
cats [x] dat state = case catH x ns of
    Left  s  -> Left s
    Right nd -> Right (nd, ns)
    where ns = write x dat state
cats (x : xs) dat state = case catH x state of
    Left  s  -> Left s
    Right nd -> cats xs (dat ++ nd) state

{-
Returns the content of a File.
-}
catH :: Name -> FSZipper -> Either String Data
catH name (Folder fname items, bs) = case file of
    Left  msg                  -> Left msg
    Right (File fname dat, bs) -> Right dat
    where file = focusFile name (Folder fname items, bs)
catH name (File fname dat, FSCrumb cname ls rs : bs) = if name == fname
    then Right dat
    else catH name (Folder cname (ls ++ [File fname dat] ++ rs), bs)

{-
Writes the given Data (dat) to a file named name. If this File does not exist
in the current focus, it is created and if the File exits already it's
overwritten.
-}
write :: Name -> Data -> FSZipper -> FSZipper
write name dat (Folder fname items, bs) = case rs of
    [] -> (Folder fname (ls ++ [File name dat] ++ rs), bs)
    (File fname fdat : items) ->
        (Folder fname (ls ++ [File fname dat] ++ items), bs)
    where (ls, rs) = break (fileNameIn name) items
write name dat (File fname fdat, FSCrumb cname ls rs : bs) = if name == fname
    then (File fname dat, bs)
    else write name dat (Folder cname (ls ++ [File fname dat] ++ rs), bs)

getInitialState :: FSZipper
getInitialState =
    ( Folder
        "/"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi"             "god bless"
        , Folder
            "pics"
            [ File "ape_throwing_up.jpg"  "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder
            "programs"
            [ File "fartwizard.exe"  "10gotofart"
            , File "owl_bandit.dmg"  "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder
                "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs"       "main = print 4"
                , Folder
                    "src"
                    [ File "best_hs_prog.hs" "main = print (fix error)"
                    , File "random.hs"       "main = print 4"
                    ]
                ]
            ]
        , File "test" "test data"
        ]
    , []
    )
