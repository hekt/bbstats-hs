module Action.Reflesh (reflesh) where

import           Database.HDBC.Session (withConnectionIO')

import           DataSource (connect)
import qualified Handler.BattingStats as BattingStats
import qualified Handler.PitchingStats as PitchingStats

reflesh :: IO ()
reflesh = withConnectionIO' connect $ \conn -> do
  BattingStats.reflesh conn
  PitchingStats.reflesh conn
  putStrLn "success"
