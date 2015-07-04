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
getListByGameId conn = fetchAll' conn . findListByGameId

getListByPlayerId :: IConnection conn => conn -> Int32 -> IO [AtBat]
getListByPlayerId conn = fetchAll' conn . findListByPlayerId

getListWithDateByPlayerId :: IConnection conn =>
                             conn -> Int32 -> IO [(AtBat, Day)]
getListWithDateByPlayerId conn = fetchAll' conn . q
    where q pid = relation $ do
                    a <- query $ findListByPlayerId pid
                    g <- query GameScore.tblGameScore
                    on $ a ! gameId' .=. g ! GameScore.id'
                    return $ a >< (g ! GameScore.gameDate')
