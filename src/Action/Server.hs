{-# LANGUAGE OverloadedStrings #-}

module Action.Server (server) where

import           Control.Monad
import           Text.Read (readMaybe)
import           Web.Scotty

import           Action.Util (json', notFoundAction)
import qualified Service.GameService as GameService
import qualified Service.StatsService as StatsService

server :: IO ()
server = scotty 52606 $ do
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
