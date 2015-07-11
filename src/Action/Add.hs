{-# LANGUAGE ScopedTypeVariables #-}

module Action.Add (addPlayer, addScore) where

import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Maybe (fromJust)
import           Data.Time.Calendar (Day)
import           Database.HDBC.Types (commit, rollback)
import           Database.HDBC.Session (withConnectionIO')
import           GHC.Int (Int16, Int32)
import           System.FilePath.Posix ((</>))

import           Action.Util
import           DataSource (connect)
import qualified DataSource as DS
import qualified Model.Player as Player
import qualified Handler.Player as Player
import qualified Handler.AtBat as AtBat
import qualified Handler.BattingResult as BattingResult
import qualified Handler.BattingStats as BattingStats
import qualified Handler.GameScore as GameScore
import qualified Handler.PitchingResult as PitchingResult
import qualified Handler.PitchingStats as PitchingStats

addPlayer :: Player.PlayerP -> IO ()
addPlayer p = withConnectionIO' connect $ \conn -> do
  result <- Player.put conn p
  commit conn

addScore :: FilePath -> IO ()
addScore path = addScoreFromCSVs $
                [ path </> DS.gameScoreCsvFileName
                , path </> DS.battingResultCsvFileName
                , path </> DS.pitchingResultCsvFileName
                , path </> DS.atBatCsvFileName
                ]

addPlayersFromCSV :: FilePath -> IO ()
addPlayersFromCSV path = withConnectionIO' connect $ \conn -> do
  Player.copyFromCSV conn path
  commit conn

addScoreFromCSVs :: [FilePath] -> IO ()
addScoreFromCSVs paths = withConnectionIO' connect $ \conn -> do
  let [gamePath, battingPath, pitchingPath, atBatPath] = paths
  result <- runExceptT $ do
    gameId   <- ExceptT $ GameScore.putFromCSVAndGetId conn gamePath
    batting  <- ExceptT $ BattingResult.putAllFromCSVWithGameId
                conn battingPath gameId
    pitching <- ExceptT $ PitchingResult.putAllFromCSVWithGameId
                conn pitchingPath gameId
    atbat    <- ExceptT $ AtBat.putAllFromCSVWithGameId conn atBatPath gameId
    return $ sum [1, batting, pitching, atbat]
  case result of
    Left msg -> rollback conn >> putStrLn msg
    Right n  -> commit conn >> print n
