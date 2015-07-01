module Handler.PitchingResult
    ( getListByGameId
    , getListWithPlayerByGameId
    ) where

import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.PitchingResult
import           Query.PitchingResult
import qualified Model.Player as Player
import qualified Query.Player as Player

getListByGameId :: (IConnection conn) => conn -> Int32 -> IO [PitchingResult]
getListByGameId conn gid =
    prepare conn (relationalQuery $ findListByGameId gid)
                >>= execute . flip bind () >>= fetchAll'

getListWithPlayerByGameId :: (IConnection conn) =>
                   conn -> Int32 -> IO [(PitchingResult, Player.Player)]
getListWithPlayerByGameId conn gid = prepare conn (relationalQuery q)
                                     >>= execute . flip bind () >>= fetchAll'
    where q = relation $ do
                q <- query $ findListByGameId gid
                p <- query Player.mstPlayer
                on $ q ! playerId' .=. p ! Player.id'
                return $ q >< p
