module Handler.GameScore
    ( get
    , getAll
    , getByDate
    ) where

import           Data.Time.Calendar (Day)
import           Database.HDBC.Types (IConnection)
import           GHC.Int (Int32)

import           Handler.Util (fetch, fetchAll')
import           Model.GameScore
import           Query.GameScore

get :: IConnection conn => conn -> Int32 -> IO (Maybe GameScore)
get conn gid = fetch conn $ find gid
    
getAll :: IConnection conn => conn -> IO [GameScore]
getAll conn = fetchAll' conn findAll

getByDate :: IConnection conn => conn -> Day -> IO (Maybe GameScore)
getByDate conn day = fetch conn $ findByDate day
