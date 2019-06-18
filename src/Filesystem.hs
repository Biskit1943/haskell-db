module Filesystem
    ()
where

import           Data.List                      ( break )

{-
An Item in the Filesystem consist either of a File with a Name and Data in it or
a Folder which consists of FSItems which can be either File or Folder and so on.
-}
type Name = String
type Data = String
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
lsH [Folder        name _ ] = [name]
lsH [File          name _ ] = [name]
lsH (Folder name _ :    xs) = name : lsH xs
lsH (File   name _ :    xs) = name : lsH xs

{-
Set a directory as new focus.

If a folder is given the left items plus the item were at and the right items
are checked if the given name matches a Folder in this list. If a matching
Folder was found the Folder will be returned with Just(a) else Nothing is
returned.
If a File was given, the Folder in which this File is, is invoked on cd.
If somehow a File was given with no FSCrumbs Nothing is returned (this, however
should never happen).
-}
cd :: Name -> FSZipper -> Maybe FSZipper
cd name (Folder fname items, bs) = case rs of
    []             -> Nothing
    (item : ritem) -> Just (item, FSCrumb fname ls ritem : bs)
    where (ls, rs) = break (folderNameIn name) items
cd name (File fname dat, FSCrumb cname ls rs : bs) =
    cd name (Folder cname (ls ++ [File fname dat] ++ rs), bs)
cd name (File _ _, []) = Nothing

-- cat :: Name -> FSZipper -> Data
-- cat name (Folder fname items, bs) = 

{-
This will check if the given Name is the name of the given FSItem. But only
Folders are matched a File will always return False
-}
folderNameIn :: Name -> FSItem -> Bool
folderNameIn name (Folder fname items) = fname == name
folderNameIn name (File   _     _    ) = False


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
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item : rs) = break (nameIn name) items
    in  (item, FSCrumb folderName ls rs : bs)

{-
This function returnes if a given Name matches the Name of an FSItem (Folder or
File).
-}
nameIn :: Name -> FSItem -> Bool
nameIn name (Folder folderName _) = name == folderName
nameIn name (File   fileName   _) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File   name dat  , bs) = (File name dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item : items), bs)

myDisk :: FSItem
myDisk = Folder
    "root"
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
