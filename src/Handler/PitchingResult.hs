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
getListByGameId conn gid = fetchAll' conn $ findListByGameId gid

getListByPlayerId ::  IConnection conn => conn -> Int32 -> IO [PitchingResult]
getListByPlayerId conn pid = fetchAll' conn $ findListByPlayerId pid

getListWithPlayerByGameId :: IConnection conn =>
                   conn -> Int32 -> IO [(PitchingResult, Player.Player)]
getListWithPlayerByGameId conn gid = fetchAll' conn q
    where q = relation $ do
                q <- query $ findListByGameId gid
                p <- query Player.mstPlayer
                on $ q ! playerId' .=. p ! Player.id'
                return $ q >< p
