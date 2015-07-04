module Handler.PitchingStats
    ( get
    , getAll
    , reflesh
    ) where

import           Database.HDBC.Types (IConnection, runRaw)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Handler.Util (fetch, fetchAll', refleshSql)
import           Model.PitchingStats
import           Query.PitchingStats
import qualified Model.Player as Player
import qualified Query.Player as Player

get :: IConnection conn => conn -> Int32 -> IO (Maybe PitchingStats)
get conn = fetch conn . find

getAll :: IConnection conn => conn -> IO [PitchingStats]
getAll conn = fetchAll' conn findAll

reflesh :: IConnection conn => conn -> IO ()
reflesh conn = runRaw conn $ refleshSql tableName
