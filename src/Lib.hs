{-# LANGUAGE ViewPatterns #-}
module Lib
    ( processInput
    ) where

import Data.List (stripPrefix)
import System.Console.Haskeline (outputStrLn)


processInput (stripPrefix "create" -> Just restOfString) = outputStrLn restOfString
processInput (stripPrefix "read" -> Just restOfString) = outputStrLn restOfString
processInput (stripPrefix "delete" -> Just restOfString) = outputStrLn restOfString
processInput string = outputStrLn string