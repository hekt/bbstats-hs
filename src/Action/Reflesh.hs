module Action.Reflesh (reflesh) where

import           Action.Util (handleSql)
import           DataSource (connect)
import qualified Handler.BattingStats as BattingStats
import qualified Handler.PitchingStats as PitchingStats

reflesh :: IO ()
reflesh = handleSql connect $ \conn -> do
  BattingStats.reflesh conn
  PitchingStats.reflesh conn
