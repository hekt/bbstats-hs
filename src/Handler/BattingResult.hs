module Handler.BattingResult
    ( getListByGameId
    , getListWithPlayerByGameId
    ) where

import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.BattingResult
import           Query.BattingResult
import qualified Model.Player as Player
import qualified Query.Player as Player

getListByGameId :: (IConnection conn) => conn -> Int32 -> IO [BattingResult]
getListByGameId conn gid =
    prepare conn (relationalQuery $ findListByGameId gid)
                >>= execute . flip bind () >>= fetchAll'

getListWithPlayerByGameId :: (IConnection conn) =>
                   conn -> Int32 -> IO [(BattingResult, Player.Player)]
getListWithPlayerByGameId conn gid = prepare conn (relationalQuery q)
                                     >>= execute . flip bind() >>= fetchAll'
    where q = relation $ do
            b <- query $ findListByGameId gid
            p <- query Player.mstPlayer
            on $ b ! playerId' .=. p ! Player.id'
            return $ b >< p
