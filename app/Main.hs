module Main where

import           System.Console.Haskeline       ( InputT
                                                , runInputT
                                                , defaultSettings
                                                , getInputLine
                                                , outputStrLn
                                                )
import           Data.List                      ( intercalate
                                                , init
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           System.Posix.User
import           Filesystem

def = "\ESC[0m\STX"
red = "\ESC[1;31m\STX"
green = "\ESC[1;32m\STX"
blue = "\ESC[1;34m\STX"
purple = "\ESC[1;35m\STX"
help =
  green
    ++ "haskell-fs"
    ++ def
    ++ " by "
    ++ blue
    ++ "Maximilian Konter\n\n"
    ++ def
    ++ "This small fake Filesystem was created with the help of this guide, which\
\\nexplained me how to use a Zipper in haskell:\n\
\http://learnyouahaskell.com/zippers\n\
\Also great thanks to\n\
\Willem Van Onsem (https://stackoverflow.com/users/67579/willem-van-onsem)\n\
\who helped me a lot on StackOverflow with my Haskell problems.\n\n\
\commands:\n\
\h/help                       -> prints this help text\n\
\cd [., ./, .., ../, <path>]  -> cd into the given directory\n\
\pwd                          -> show the path to the current directory\n\
\ls                           -> lists the contens of the current directory\n\
\mkdir <dname>                -> create a directory named <dname>\n\
\rmdir [<dname>]              -> removes <dname>, if it's empty. If there are\n\
\                                whitespaces in the name, you must escape them\n\
\                                for a match with \\\n\
\write \"<data>\" <fname>       -> writes <data> to a file named <fname> if\n\
\                                <fname> already exists, is's overwritten\n\
\cat <fname>                  -> prints the contens of a file\n\
\cat [<fname>]                -> write the contents of all but the last to the\n\
\                                last file\n\
\q/quit/exit                  -> exit the cli"


processCommand :: FSZipper -> String -> Either String (FSZipper, Maybe String)
processCommand state "ls" =
  Left $ intercalate "\n" $ prittyLs $ sortFSItem $ lsItem state

-- cd -> back to /
processCommand state "cd" = Right (getRoot $ bd state, Nothing)

-- cd . || cd ./ -> cd to current Folder (useless but its possible)
processCommand state "cd ."                 = Right (state, Nothing)
processCommand state "cd ./"                = Right (state, Nothing)

-- cd .. | cd ../ -> "bd" one directory up
processCommand state "cd .."                = Right (bd state, Nothing)
processCommand state "cd ../"               = Right (bd state, Nothing)

-- cd <dname> -> cd into directory <dname>
processCommand state ('c' : 'd' : ' ' : xs) = case maybe_msg of
  Left  s  -> Left $ red ++ s ++ def
  Right ns -> Right (ns, Nothing)
  where maybe_msg = cd xs state

-- pwd -> print path to current directory
processCommand (focus, []) "pwd" = Left $ pwd (focus, [])
processCommand state       "pwd" = Left $ init $ pwd state

-- mkdir <dname> -> create directory <dname>
processCommand state ('m' : 'k' : 'd' : 'i' : 'r' : ' ' : xs) =
  case mkdir xs state of
    Left  s  -> Left $ red ++ s ++ def
    Right ns -> Right (ns, Nothing)

-- rmdir <dname> -> removes directory <dname> (if empty)
processCommand state ('r' : 'm' : 'd' : 'i' : 'r' : ' ' : xs) =
  case rmdir xs state of
    Left  s  -> Left $ red ++ s ++ def
    Right ns -> Right (ns, Nothing)

-- write "<data>" <fname> -> write <data> to file <fname> (overwrites existing files)
processCommand state ('w' : 'r' : 'i' : 't' : 'e' : ' ' : xs) =
  case head dat of
    '"' -> case last dat of
      '"' -> Right (write name (init $ tail dat) state, Nothing)
      _ ->
        Left
          $  red
          ++ "failed to parse input (leading and trainling '\"' missing)"
    _ ->
      Left $ red ++ "failed to parse input (leading and trainling '\"' missing)"
  where (dat, ' ' : name) = break (== ' ') xs

-- cat <fname> -> prints the contens of the file <fname>
processCommand state ('c' : 'a' : 't' : ' ' : xs) = case cat xs state of
  Left  s         -> Left $ red ++ s ++ def
  Right (dat, ns) -> Right (ns, Just (dat))

-- rm <fname> -> removes the file <fname>
processCommand state ('r' : 'm' : ' ' : xs) = case rm xs state of
  Left  s  -> Left $ red ++ s ++ def
  Right ns -> Right (ns, Nothing)

processCommand _ command = Left $ red ++ "unknown command: " ++ command ++ def

{-
Pritty printes the contens of a directory with colored output
-}
prittyLs :: [FSItem] -> [String]
prittyLS [] = [""]
prittyLs [Folder        name _ ] = [purple ++ name ++ def]
prittyLs [File          name _ ] = [blue ++ name ++ def]
prittyLs (Folder name _ :    xs) = (purple ++ name ++ def) : prittyLs xs
prittyLs (File   name _ :    xs) = (blue ++ name ++ def) : prittyLs xs

main :: IO ()
main = runInputT defaultSettings $ loop initialState
 where
  initialState = getInitialState
  loop :: FSZipper -> InputT IO ()
  loop state = do
    user <- liftIO getEffectiveUserName
    let path           = pwd state
    let truncated_path = if length path > 1 then init path else path
    minput <-
      getInputLine
      $  user
      ++ purple
      ++ " at "
      ++ def
      ++ truncated_path
      ++ green
      ++ " $ "
      ++ def
    case minput of
      Nothing     -> loop state
      Just "q"    -> return ()
      Just "quit" -> return ()
      Just "exit" -> return ()
      Just "h"    -> do
        outputStrLn help
        loop state
      Just "help" -> do
        outputStrLn help
        loop state
      Just string -> case processCommand state string of
        Left err -> do
          outputStrLn err
          loop state
        Right (ns, msg) -> case msg of
          Just m -> do
            outputStrLn m
            loop ns
          Nothing -> loop ns
