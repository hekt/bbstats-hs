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
import qualified Service.GameService as GameService
import qualified Service.StatsService as StatsService

main :: IO ()
main = scotty 52606 $ do
  get "/api/list" gameListAction
  get "/api/detail/:gid" $ read <$> param "gid" >>= gameDetailAction
  get "/api/stats/batting" $ battingStatsAction
  get "/api/stats/pitching" $ pitchingStatsAction
  get "/api/stats/:pid" $ read <$> param "pid" >>= playerStatsAction

gameListAction = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO GameService.gameSummaryList

gameDetailAction gid = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO (GameService.gameDetail gid)

battingStatsAction = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO StatsService.battingStats

pitchingStatsAction = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO StatsService.pitchingStats

playerStatsAction pid = do
  setHeader "content-type" "application/json; charset=UFT-8"
  json =<< liftIO (StatsService.playerStats pid)
