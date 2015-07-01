{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Aeson (toJSON, object, (.=))
import           Data.Aeson.Encode.Pretty
import qualified Data.Map as Map
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           GHC.Int (Int32)
import           Web.Scotty
import           System.Environment (getArgs)

import           DataSource (connect)
import           Service.GameService as Service

main :: IO ()
main = scotty 52606 $ do
  get "/api/list" listAction
  get "/api/detail/:gid" $ read <$> param "gid" >>= detailAction

listAction = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO Service.gameSummaryList

detailAction gid = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO (Service.gameDetail gid)
