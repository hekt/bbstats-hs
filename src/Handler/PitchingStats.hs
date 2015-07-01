module Handler.PitchingStats
    ( get
    , getAll
    ) where

import           Database.HDBC.Record.Query (runQuery)
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)
import           Safe (headMay)

import           Model.PitchingStats
import           Query.PitchingStats

get :: IConnection conn => conn -> Int32 -> IO (Maybe PitchingStats)
get conn pid = prepare conn (relationalQuery $ fetch pid)
               >>= execute . flip bind () >>= fetch

getAll :: IConnection conn => conn -> IO [PitchingStats]
getAll conn = prepare conn (relationalQuery fetchAll)
              >>= execute . flip bind () >>= fetchAll'
