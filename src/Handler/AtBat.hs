module Handler.AtBat
    ( getListByGameId
    , getListByPlayerId
    , getListWithDateByPlayerId
    ) where

import           Data.Time.Calendar (Day)
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Handler.Util (fetch, fetchAll')
import           Model.AtBat
import           Query.AtBat
import qualified Model.GameScore as GameScore
import qualified Query.GameScore as GameScore

getListByGameId :: IConnection conn => conn -> Int32 -> IO [AtBat]
getListByGameId conn gid = fetchAll' conn $ findListByGameId gid

getListByPlayerId :: IConnection conn => conn -> Int32 -> IO [AtBat]
getListByPlayerId conn pid = fetchAll' conn $ findListByPlayerId pid

getListWithDateByPlayerId :: IConnection conn =>
                             conn -> Int32 -> IO [(AtBat, Day)]
getListWithDateByPlayerId conn pid = fetchAll' conn q
    where q = relation $ do
                a <- query $ findListByPlayerId pid
                g <- query GameScore.tblGameScore
                on $ a ! gameId' .=. g ! GameScore.id'
                return $ a >< (g ! GameScore.gameDate')
