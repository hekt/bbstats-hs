module Handler.GameScore
    ( get
    , getAll
    , getByDateAndNumber
    , put
    ) where

import           Data.Time.Calendar (Day)
import           Database.HDBC.Record (runInsertQuery)
import           Database.HDBC.Types (IConnection)
import           GHC.Int (Int16, Int32)

import           Handler.Util (fetch, fetchAll')
import           Model.GameScore
import           Query.GameScore

get :: IConnection conn => conn -> Int32 -> IO (Maybe GameScore)
get conn = fetch conn .  find
    
getAll :: IConnection conn => conn -> IO [GameScore]
getAll conn = fetchAll' conn findAll

getByDateAndNumber :: IConnection conn
                   => conn -> Day -> Int16 -> IO (Maybe GameScore)
getByDateAndNumber conn d = fetch conn . findByDateAndNumber d

put :: IConnection conn => conn -> GameScoreP -> IO Integer
put conn g = runInsertQuery conn (persist g) ()
