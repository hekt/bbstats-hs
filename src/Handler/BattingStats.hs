module Handler.BattingStats
    ( get
    , getAll
    ) where

import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)
import           Safe (headMay)

import           Model.BattingStats
import           Query.BattingStats

get :: IConnection conn => conn -> Int32 -> IO (Maybe BattingStats)
get conn pid = prepare conn (relationalQuery $ find pid)
               >>= execute . flip bind () >>= fetch

getAll :: IConnection conn => conn -> IO [BattingStats]
getAll conn = prepare conn (relationalQuery findAll)
              >>= execute . flip bind () >>= fetchAll'
