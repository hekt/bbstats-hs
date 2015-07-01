module Handler.AtBat
    ( getListByGameId
    , getListByPlayerId
    , getListWithDateByPlayerId
    ) where

import           Data.Time.Calendar (Day)
import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.AtBat
import           Query.AtBat
import qualified Model.GameScore as GameScore
import qualified Query.GameScore as GameScore

getListByGameId :: (IConnection conn) => conn -> Int32 -> IO [AtBat]
getListByGameId conn gid =
    prepare conn (relationalQuery $ findListByGameId gid)
                >>= execute . flip bind () >>= fetchAll'

getListByPlayerId :: (IConnection conn) => conn -> Int32 -> IO [AtBat]
getListByPlayerId conn pid =
    prepare conn (relationalQuery $ findListByPlayerId pid)
                >>= execute . flip bind () >>= fetchAll'

getListWithDateByPlayerId :: (IConnection conn) =>
                             conn -> Int32 -> IO [(AtBat, Day)]
getListWithDateByPlayerId conn pid = prepare conn (relationalQuery q)
                                     >>= execute . flip bind () >>= fetchAll'
    where q = relation $ do
                a <- query $ findListByPlayerId pid
                g <- query GameScore.tblGameScore
                on $ a ! gameId' .=. g ! GameScore.id'
                return $ a >< (g ! GameScore.gameDate')
