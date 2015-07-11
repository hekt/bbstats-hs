module Handler.PitchingResult
    ( getListByGameId
    , getListByPlayerId
    , getListWithPlayerByGameId
    , put
    , putAllFromCSVWithGameId
    ) where

import           Database.HDBC.Record (runInsertQuery)
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Handler.Util
import           Model.PitchingResult
import           Query.PitchingResult
import qualified Model.Player as Player
import qualified Query.Player as Player

getListByGameId :: IConnection conn => conn -> Int32 -> IO [PitchingResult]
getListByGameId conn = fetchAll' conn . findListByGameId

getListByPlayerId ::  IConnection conn => conn -> Int32 -> IO [PitchingResult]
getListByPlayerId conn = fetchAll' conn . findListByPlayerId

getListWithPlayerByGameId :: IConnection conn => conn
                          -> Int32 -> IO [(PitchingResult, Player.Player)]
getListWithPlayerByGameId conn = fetchAll' conn . q
    where q gid = relation $ do
                    q <- query $ findListByGameId gid
                    p <- query Player.mstPlayer
                    on $ q ! playerId' .=. p ! Player.id'
                    return $ q >< p

put :: IConnection conn => conn -> PitchingResultP -> IO Integer
put conn p = runInsertQuery conn (persist p) ()

putAllFromCSVWithGameId :: IConnection conn => conn
                        -> FilePath -> Int32 -> IO (Either String Integer)
putAllFromCSVWithGameId conn path gid =
  putAllFromCSVWithPutAction path $ \result -> put conn result {pGameId = gid}
