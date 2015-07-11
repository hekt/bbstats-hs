module Handler.BattingResult
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
import           Model.BattingResult
import           Query.BattingResult
import qualified Model.Player as Player
import qualified Query.Player as Player

getListByGameId :: IConnection conn => conn -> Int32 -> IO [BattingResult]
getListByGameId conn = fetchAll' conn . findListByGameId

getListByPlayerId ::  IConnection conn => conn -> Int32 -> IO [BattingResult]
getListByPlayerId conn = fetchAll' conn . findListByPlayerId

getListWithPlayerByGameId :: IConnection conn =>
                   conn -> Int32 -> IO [(BattingResult, Player.Player)]
getListWithPlayerByGameId conn = fetchAll' conn . q
    where q gid = relation $ do
            b <- query $ findListByGameId gid
            p <- query Player.mstPlayer
            on $ b ! playerId' .=. p ! Player.id'
            return $ b >< p

put :: IConnection conn => conn -> BattingResultP -> IO Integer
put conn b = runInsertQuery conn (persist b) ()

putAllFromCSVWithGameId :: IConnection conn => conn
                        -> FilePath -> Int32 -> IO (Either String Integer)
putAllFromCSVWithGameId conn path gid =
  putAllFromCSVWithPutAction path $ \result -> put conn result {pGameId = gid}
