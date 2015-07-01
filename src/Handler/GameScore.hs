module Handler.GameScore
    ( get
    , getAll
    , getByDate
    ) where

import           Data.Time.Calendar (Day)
import           Safe (headMay)

import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query (relationalQuery)
import           GHC.Int (Int32)

import           Query.GameScore
import           Model.GameScore

get :: IConnection conn => conn -> Int32 -> IO (Maybe GameScore)
get conn gid = prepare conn (relationalQuery $ find gid)
               >>= execute . flip bind () >>= fetch
    
getAll :: IConnection conn => conn -> IO [GameScore]
getAll conn = prepare conn (relationalQuery findAll)
              >>= execute . flip bind () >>= fetchAll'

getByDate :: IConnection conn => conn -> Day -> IO (Maybe GameScore)
getByDate conn day = prepare conn (relationalQuery $ findByDate day)
                     >>= execute . flip bind () >>= fetch
