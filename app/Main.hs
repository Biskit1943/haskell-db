module Main where

import System.Console.Haskeline
import Lib

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "\ESC[1;32m\STX$\ESC[0m\STX "
            case minput of
                Nothing -> loop
                Just "q" -> return()
                Just "quit" -> return()
                Just string -> do processInput string
                                  loop
