module Handler.Player
    ( get
    , getAll
    , getByName
    , getByNumber
    , getPlayerNameMap
    , put
    ) where

import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)
import           Database.HDBC.Record (runInsertQuery)
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query hiding (isJust)
import           GHC.Int (Int32)

import           Handler.Util (fetch, fetchAll')
import           Model.Player
import           Query.Player

get :: IConnection conn => conn -> Int32 -> IO (Maybe Player)
get conn = fetch conn . find

getAll :: IConnection conn => conn -> IO [Player]
getAll conn = fetchAll' conn findAll

getByName :: IConnection conn => conn -> String -> IO (Maybe Player)
getByName conn = fetch conn . findByName

getByNumber :: IConnection conn => conn -> String -> IO (Maybe Player)
getByNumber conn = fetch conn . findByNumber

getPlayerNameMap :: IConnection conn => conn -> IO (Map.Map String String)
getPlayerNameMap conn = getAll conn >>= return . Map.fromList . toTuples
    where
      toTuples [] = []
      toTuples (p:ps)
          | isJust $ uniformNumber p =
              (fromJust $ uniformNumber p, playerName p)
              : toTuples ps
          | isJust $ tempUniformNumber p =
              (fromJust $ uniformNumber p, playerName p)
              : toTuples ps
          | otherwise = toTuples ps

put :: IConnection conn => conn -> PlayerP -> IO Integer
put conn p = runInsertQuery conn (persist p) ()
