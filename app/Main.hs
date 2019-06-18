module Main where

import           Database                       ( getInitialState
                                                , processInput
                                                , DB
                                                , TVar
                                                )
import           System.Console.Haskeline       ( InputT
                                                , runInputT
                                                , defaultSettings
                                                , getInputLine
                                                )
import           Control.Monad.IO.Class         ( liftIO )

main :: IO ()
main = do
  database <- liftIO getInitialState
  runInputT defaultSettings $ loop database
  where
    loop :: TVar DB -> InputT IO ()
    loop db = do
        minput <- getInputLine "\ESC[1;32m\STX$\ESC[0m\STX "
        case minput of
            Nothing     -> loop db
            Just "q"    -> return ()
            Just "quit" -> return ()
            Just string -> do
                processInput string db
                loop db
