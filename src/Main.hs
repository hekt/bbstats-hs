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
import qualified Service.GameService as GameService
import qualified Service.StatsService as StatsService

main :: IO ()
main = scotty 52606 $ do
  get "/api/game/list"        gameListAction
  get "/api/game/:gid"      $ param "gid" >>= gameDetailAction
  get "/api/stats/batting"    battingStatsAction
  get "/api/stats/pitching"   pitchingStatsAction
  get "/api/stats/:pid"     $ param "pid" >>= playerStatsAction

gameListAction :: ActionM ()
gameListAction = json' GameService.gameSummaryList

gameDetailAction :: String -> ActionM ()
gameDetailAction = maybe notFoundAction act . readMaybe
    where act gid = json' $ GameService.gameDetail gid

battingStatsAction :: ActionM ()
battingStatsAction = json' StatsService.battingStats

pitchingStatsAction :: ActionM ()
pitchingStatsAction = json' StatsService.pitchingStats

playerStatsAction :: String -> ActionM ()
playerStatsAction = maybe notFoundAction act . readMaybe
    where act pid = json' $ StatsService.playerStats pid
