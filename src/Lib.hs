{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( processInput
  , create
  , atomRead
  )
where

import           Data.ByteString.Char8          ( ByteString
                                                , pack
                                                )
import           Control.Concurrent.STM
import           Data.List                      ( stripPrefix )
import           Data.Map                       ( fromList
                                                , findWithDefault
                                                , Map
                                                , insert
                                                )
import           System.Console.Haskeline       ( outputStrLn )

version :: ByteString
version = "0.5.0"

type Value = ByteString
type Key = ByteString
type DB = Map Key Value

data Command = Get Key
             | Set Key Value
             | Unknown
             deriving (Eq, Show)

atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

getValue :: DB -> Key -> Value
getValue db k = findWithDefault "null" k db

updateValue :: (DB -> DB) -> TVar DB -> IO ()
updateValue fn x = atomically $ modifyTVar x fn

create = do
  database <- atomically $ newTVar $ fromList [("__version__", version)]
  m        <- atomRead database
  value    <- getValue m "__version__"
  return value

processInput (stripPrefix "create" -> Just restOfString) =
  outputStrLn restOfString
processInput (stripPrefix "read" -> Just restOfString) =
  outputStrLn restOfString
processInput (stripPrefix "update" -> Just restOfString) =
  outputStrLn restOfString
processInput (stripPrefix "delete" -> Just restOfString) =
  outputStrLn restOfString
processInput string = outputStrLn string
