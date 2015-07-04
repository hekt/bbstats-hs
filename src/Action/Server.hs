{-# LANGUAGE OverloadedStrings #-}

module Action.Server (server) where

import           Control.Monad
import           Data.Aeson hiding (json')
import           Text.Read (readMaybe)
import           Web.Scotty

import           DataSource (connect)
import           Action.Util (json', handleSql, notFoundAction)
import qualified Handler.AtBat as AtBat
import qualified Handler.BattingResult as Batting
import qualified Handler.BattingStats as BattingStats
import qualified Handler.GameScore as Score
import qualified Handler.PitchingResult as Pitching
import qualified Handler.PitchingStats as PitchingStats
import qualified Handler.Player as Player

server :: IO ()
server = scotty 52606 $ do
  get "/api/game/list"        listAction
  get "/api/game/:gid"      $ param "gid" >>= detailAction
  get "/api/stats/batting"    battingStatsAction
  get "/api/stats/pitching"   pitchingStatsAction
  get "/api/stats/:pid"     $ param "pid" >>= playerStatsAction

listAction :: ActionM ()
listAction = json' $ handleSql connect $ \conn ->
  Score.getAll conn >>= return . toJSON

detailAction :: String -> ActionM ()
detailAction = maybe notFoundAction act . readMaybe where
  act gid = json' $ handleSql connect $ \conn -> do
    atbats    <- AtBat.getListByGameId conn gid
    score     <- Score.get conn gid
    battings  <- Batting.getListWithPlayerByGameId conn gid
    pitchings <- Pitching.getListWithPlayerByGameId conn gid
    return $ object [ "score"     .= toJSON score
                    , "atbats"    .= toJSON atbats
                    , "battings"  .= toJSON battings
                    , "pitchings" .= toJSON pitchings
                    ]

battingStatsAction :: ActionM ()
battingStatsAction = json' $ handleSql connect $ \conn ->
  BattingStats.getAll conn >>= return . toJSON

pitchingStatsAction :: ActionM ()
pitchingStatsAction = json' $ handleSql connect $ \conn ->
  PitchingStats.getAll conn >>= return . toJSON

playerStatsAction :: String -> ActionM ()
playerStatsAction = maybe notFoundAction act . readMaybe where
  act pid = json' $ handleSql connect $ \conn -> do
    p   <- Player.get conn pid
    bss <- BattingStats.get conn pid
    pss <- PitchingStats.get conn pid
    return $ object [ "player"           .= toJSON p
                    , "batting_stats"    .= toJSON bss
                    , "pitching_stats"   .= toJSON pss
                    ]
