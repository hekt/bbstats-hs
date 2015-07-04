module Handler.BattingStats
    ( get
    , getAll
    ) where

import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Handler.Util (fetch, fetchAll')
import           Model.BattingStats
import           Query.BattingStats
import qualified Model.Player as Player
import qualified Query.Player as Player

get :: IConnection conn => conn -> Int32 -> IO (Maybe BattingStats)
get conn = fetch conn . find

getAll :: IConnection conn => conn -> IO [BattingStats]
getAll conn = fetchAll' conn findAll
