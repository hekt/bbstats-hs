module Handler.PitchingResult
    ( getListByGameId
    , getListByPlayerId
    , getListWithPlayerByGameId
    ) where

import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Handler.Util (fetch, fetchAll')
import           Model.PitchingResult
import           Query.PitchingResult
import qualified Model.Player as Player
import qualified Query.Player as Player

getListByGameId :: IConnection conn => conn -> Int32 -> IO [PitchingResult]
getListByGameId conn = fetchAll' conn . findListByGameId

getListByPlayerId ::  IConnection conn => conn -> Int32 -> IO [PitchingResult]
getListByPlayerId conn = fetchAll' conn . findListByPlayerId

getListWithPlayerByGameId :: IConnection conn =>
                   conn -> Int32 -> IO [(PitchingResult, Player.Player)]
getListWithPlayerByGameId conn = fetchAll' conn . q
    where q gid = relation $ do
                    q <- query $ findListByGameId gid
                    p <- query Player.mstPlayer
                    on $ q ! playerId' .=. p ! Player.id'
                    return $ q >< p
