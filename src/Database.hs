{-# LANGUAGE OverloadedStrings #-}  -- Needed to parse String to ByteString
module Database
  ( processInput
  , getInitialState
  , DB
  , TVar
  )
where

import           Data.List.Split                ( splitOn )
import           Control.Monad.IO.Class         ( liftIO )
import           System.Console.Haskeline       ( InputT
                                                , outputStrLn
                                                )
import qualified Data.Map as Map
import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , newTVar
                                                , readTVar
                                                , modifyTVar
                                                , readTVarIO
                                                )
import           Data.ByteString.UTF8          as BSU
import           Data.ByteString               as BS
import           Text.Read                      ( readMaybe )

type Value = ByteString
type Key = ByteString
type DB = Map.Map Key Value

data Command = Get Key
             | Set Key Value
             | Update Key Value
             deriving (Eq, Show)

help :: String
help =
  "haskell-db - A in-memory database written in Haskell.\n\
\\n\
\Commands:\n\
\get    <key>\n\
\set    <key> <value>\n\
\update <key> <value>\n\n"

version :: ByteString
version = "0.1.0.0"

getInitialState :: IO (TVar DB)
getInitialState = atomically $ newTVar $ Map.fromList [("__version__", version)]

atomRead :: TVar a -> IO a
atomRead = readTVarIO

updateValue :: (DB -> DB) -> TVar DB -> IO ()
updateValue fn x = atomically $ modifyTVar x fn

getValue :: DB -> Key -> Value
getValue db k = Map.findWithDefault "null" k db

----------------------------------parseArgs------------------------------------
parseArgs :: [String] -> Either String Command
parseArgs ["get", k] = Right $ Get $ BSU.fromString k
parseArgs ["get"]    = Left "To few arguments for 'get', <key> is needed"
parseArgs ("get" : _ : _) =
  Left "To much arguments for 'get', only <key> is accepted"

parseArgs ["set", k, v] = Right $ Set (BSU.fromString k) (BSU.fromString v)
parseArgs ("set" : _ : _ : _) =
  Left "To much arguments for 'set', only <key> and <value> are accepted."
parseArgs ("set" : _) =
  Left "To few arguments for 'set', <key> and <value> are needed."

parseArgs ["update", k, v] =
  Right $ Update (BSU.fromString k) (BSU.fromString v)
parseArgs ("update" : _ : _ : _) =
  Left "To much arguments for 'update', only <key> and <value> are accepted."
parseArgs ("update" : _) =
  Left "To few arguments for 'update', <key> and <value> are needed."

parseArgs []       = Left "Nothing to do here"
parseArgs (x : xs) = Left $ "Unknown Command: " ++ x
----------------------------------parseArgs------------------------------------

executeCommand :: TVar DB -> Command -> InputT IO ()
executeCommand db (Get k) = do
  m <- liftIO $ atomRead db
  let value = getValue m k
  outputStrLn $ BSU.toString value

executeCommand db (Set k v) = do
  liftIO $ updateValue (Map.insert k v) db
  m <- liftIO $ atomRead db
  mapM_ (\(k, v) -> outputStrLn $ BSU.toString k ++ ":" ++ BSU.toString v) (Map.toList m)

executeCommand db (Update k v) = outputStrLn "executing update"

processInput :: String -> TVar DB -> InputT IO ()
processInput command db = do
  let args    = splitOn " " command
  let command = parseArgs args
  case command of
    Right c -> executeCommand db c
    Left  e -> outputStrLn $ help ++ e
