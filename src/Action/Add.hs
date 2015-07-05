{-# LANGUAGE ScopedTypeVariables #-}

module Action.Add (addPlayer) where

import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Except
import           Data.Maybe (fromJust)
import           Data.Time.Calendar (Day)
import           Database.HDBC.Types (commit)
import           Database.HDBC.Session (withConnectionIO')
import           GHC.Int (Int16, Int32)

import           DataSource (connect)
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

addScoreFromCsvs :: Day -> Int16 -> [FilePath] -> IO ()
addScoreFromCsvs date num paths = withConnectionIO' connect $ \conn -> do
  let [gamePath, battingPath] = paths

  GameScore.copyFromCSV conn gamePath
  result <- runExceptT $ do
    gameId <- ExceptT $
              maybe (Left "failred to insert the game score") Right
              <$> GameScore.getGameIdByDateAndNumber conn date num
    result <- ExceptT $
              BattingResult.putAllFromCSVWithGameId conn battingPath gameId
    liftIO $ commit conn
    return result
  case result of
    Left msg -> putStrLn msg
    Right n  -> print $ n + 1
