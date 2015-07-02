{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Service.StatsService where

import           Data.Aeson
import           GHC.Int (Int32)

import           Service.Util (handle)
import           DataSource (connect)
import qualified Handler.BattingResult as BattingResult
import qualified Handler.BattingStats as BattingStats
import qualified Handler.PitchingResult as PitchingResult
import qualified Handler.PitchingStats as PitchingStats

battingStats :: IO Value
battingStats = handle connect $ \conn -> do
  stats <- BattingStats.getAll conn
  return $ toJSON stats

pitchingStats :: IO Value
pitchingStats = handle connect $ \conn -> do
  stats <- PitchingStats.getAll conn
  return $ toJSON stats

playerStats :: Int32 -> IO Value
playerStats pid = handle connect $ \conn -> do
  brs <- BattingResult.getListByPlayerId conn pid
  bss <- BattingStats.get conn pid
  prs <- PitchingResult.getListByPlayerId conn pid
  pss <- PitchingStats.get conn pid

  return $ object [ "batting_results"  .= toJSON brs
                  , "batting_stats"    .= toJSON bss
                  , "pitching_results" .= toJSON prs
                  , "pitching_stats"   .= toJSON pss
                  ]
