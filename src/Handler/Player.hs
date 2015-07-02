module Handler.Player
    ( getAll
    , getByName
    , getByNumber
    , getPlayerNameMap
    ) where

import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query

import           Handler.Util (fetch, fetchAll')
import           Model.Player
import           Query.Player

getAll :: IConnection conn => conn -> IO [Player]
getAll conn = fetchAll' conn fetchAll

getByName :: IConnection conn => conn -> String -> IO (Maybe Player)
getByName conn name = fetch conn $ fetchByName name

getByNumber :: IConnection conn => conn -> String -> IO (Maybe Player)
getByNumber conn numStr = fetch conn $ fetchByNumber name

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
