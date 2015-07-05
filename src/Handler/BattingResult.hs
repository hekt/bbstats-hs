module Handler.BattingResult
    ( getListByGameId
    , getListByPlayerId
    , getListWithPlayerByGameId
    , put
    , putAllFromCSVWithGameId
    ) where

import           Control.Monad.Trans (lift, liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.ByteString.Lazy as BS
import           Data.Csv (decodeByName)
import qualified Data.Vector as V
import           Database.HDBC.Record (runInsertQuery)
import           Database.HDBC.Types (IConnection, runRaw)
import           Database.Relational.Query
import           GHC.Int (Int32)
import           System.Directory (makeAbsolute)

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

putAllFromCSVWithGameId :: IConnection conn
                        => conn -> FilePath -> Int32
                        -> IO (Either String Integer)
putAllFromCSVWithGameId conn path gid = runExceptT $ do
  csvBs        <- liftIO $ BS.readFile path
  (_, records) <- ExceptT . return $ decodeByName csvBs
  affecteds    <- liftIO $ V.forM records $ \result ->
    put conn result {pGameId = gid}
  return $ V.sum affecteds

copyFromCSV :: IConnection conn => conn -> FilePath -> IO ()
copyFromCSV conn path = do
  absPath <- makeAbsolute path
  runRaw conn $ copySql tableName insertColumnNames absPath
