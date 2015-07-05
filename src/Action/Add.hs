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

addScore :: FilePath -> IO (Maybe ())
addScore path = runMaybeT $ do
  (day, num) <- MaybeT $ getDateAndNumberFromFilePath path
  let scorePath = path </> DS.gameScoreCsvFileName
      battingPath = path </> DS.battingResultCsvFileName
  liftIO $ addScoreFromCSVs day num [scorePath, battingPath]

addPlayersFromCSV :: FilePath -> IO ()
addPlayersFromCSV path = withConnectionIO' connect $ \conn -> do
  Player.copyFromCSV conn path
  commit conn

addScoreFromCSVs :: Day -> Int16 -> [FilePath] -> IO ()
addScoreFromCSVs date num paths = withConnectionIO' connect $ \conn -> do
  let [gamePath, battingPath] = paths
  GameScore.copyFromCSV conn gamePath
  result <- runExceptT $ do
    gameId <- ExceptT $
              maybe (Left "failred to insert the game score") Right
              <$> GameScore.getGameIdByDateAndNumber conn date num
    result <- ExceptT $
              BattingResult.putAllFromCSVWithGameId conn battingPath gameId
    return result
  case result of
    Left msg -> rollback conn >> putStrLn msg
    Right n  -> commit conn >> print (n + 1)
