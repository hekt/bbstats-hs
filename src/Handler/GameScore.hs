module Handler.GameScore
    ( getAll
    , getByDate
    ) where

import           Data.Time.Calendar (Day)
import           Safe (headMay)

import           Database.Relational.Query (relationalQuery)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Record.Query (runQuery)
import           Database.HDBC.Types (IConnection)

import qualified Query.GameScore as Query
import qualified Model.GameScore as Model

getAll :: IConnection conn => conn -> IO [Model.GameScore]
getAll conn = runQuery conn (relationalQuery Query.fetchAll) ()

getByDate :: IConnection conn => conn -> Day -> IO (Maybe Model.GameScore)
getByDate conn day =
    (runQuery conn (relationalQuery $ Query.fetchByDate day) ())
    >>= return . headMay
