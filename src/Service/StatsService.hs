{-# LANGUAGE OverloadedStrings #-}

module Service.StatsService where

import           Data.Aeson
import           GHC.Int (Int32)

import           Service.Util (handle)
import           DataSource (connect)
import qualified Handler.Player as Player
import qualified Handler.BattingStats as BattingStats
import qualified Handler.PitchingStats as PitchingStats

battingStats :: IO Value
battingStats = handle connect $
               \conn -> BattingStats.getAll conn >>= return . toJSON

pitchingStats :: IO Value
pitchingStats = handle connect $
                \conn -> PitchingStats.getAll conn >>= return . toJSON

playerStats :: Int32 -> IO Value
playerStats pid = handle connect $ \conn -> do
  p   <- Player.get conn pid
  bss <- BattingStats.get conn pid
  pss <- PitchingStats.get conn pid

  return $ object [ "player"           .= toJSON p
                  , "batting_stats"    .= toJSON bss
                  , "pitching_stats"   .= toJSON pss
                  ]
