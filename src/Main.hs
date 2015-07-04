{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Aeson (Value, toJSON, object, (.=))
import           Data.Aeson.Encode.Pretty
import qualified Data.Map as Map
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           GHC.Int (Int32)
import           Text.Read (readMaybe)
import           System.Environment (getArgs)
import           Web.Scotty

import           Action.Util (json', notFoundAction)
import           DataSource (connect)
import           Action.Server
import           Action.Command

main :: IO ()
main = do
  (commandName:args) <- getArgs
  case commandName of
    "server"  -> server
    "command" -> command args
    _         -> command []
