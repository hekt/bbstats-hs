module Action.Add (addPlayer) where

import           Database.HDBC.Types (commit)
import           Database.HDBC.Session (withConnectionIO')

import           DataSource (connect)
import qualified Model.Player as Player
import qualified Handler.Player as Player
import qualified Handler.AtBat as AtBat
import qualified Handler.BattingResult as Batting
import qualified Handler.BattingStats as BattingStats
import qualified Handler.GameScore as Score
import qualified Handler.PitchingResult as Pitching
import qualified Handler.PitchingStats as PitchingStats

addPlayer :: Player.PlayerP -> IO ()
addPlayer p = withConnectionIO' connect $ \conn -> do
  result <- Player.put conn p
  commit conn

-- addScoreFromCsvs :: [FilePath] -> IO ()
-- addScoreFromCsvs paths = do
--   let [gamePath, battingPath, pitchingPath, atbatPath] = paths
