module Handler.PitchingStats
    ( get
    , getAll
    ) where

import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Handler.Util (fetch, fetchAll')
import           Model.PitchingStats
import           Query.PitchingStats
import qualified Model.Player as Player
import qualified Query.Player as Player

get :: IConnection conn => conn -> Int32 -> IO (Maybe PitchingStats)
get conn pid = fetch conn $ find pid

getAll :: IConnection conn => conn -> IO [PitchingStats]
getAll conn = fetchAll' conn findAll
