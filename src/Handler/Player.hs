module Handler.Player
    ( getAll
    , getByName
    , getByNumber
    , getPlayerNameMap
    ) where

import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)
import           Safe (headMay)

import           Database.Relational.Query (relationalQuery)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Record.Query (runQuery)
import           Database.HDBC.Types (IConnection)

import qualified Query.Player as Query
import qualified Model.Player as Model

getAll :: IConnection conn => conn -> IO [Model.Player]
getAll conn = runQuery conn (relationalQuery Query.fetchAll) ()

getByName :: IConnection conn => conn -> String -> IO (Maybe Model.Player)
getByName conn name =
    (runQuery conn (relationalQuery $ Query.fetchByNumber name) ())
    >>= return . headMay

getByNumber :: IConnection conn => conn -> String -> IO (Maybe Model.Player)
getByNumber conn numStr =
    (runQuery conn (relationalQuery $ Query.fetchByNumber numStr) ())
    >>= return . headMay

getPlayerNameMap :: IConnection conn => conn -> IO (Map.Map String String)
getPlayerNameMap conn = getAll conn >>= return . Map.fromList . toTuples
    where
      toTuples [] = []
      toTuples (p:ps)
          | isJust $ Model.uniformNumber p =
              (fromJust $ Model.uniformNumber p, Model.playerName p)
              : toTuples ps
          | isJust $ Model.tempUniformNumber p =
              (fromJust $ Model.uniformNumber p, Model.playerName p)
              : toTuples ps
          | otherwise = toTuples ps
