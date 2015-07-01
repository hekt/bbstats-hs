module Handler.Player
    ( getAll
    , getByName
    , getByNumber
    , getPlayerNameMap
    ) where

import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)
import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query
import           Safe (headMay)

import           Model.Player
import           Query.Player

getAll :: IConnection conn => conn -> IO [Player]
getAll conn = prepare conn (relationalQuery fetchAll)
              >>= execute . flip bind () >>= fetchAll'

getByName :: IConnection conn => conn -> String -> IO (Maybe Player)
getByName conn name = prepare conn (relationalQuery $ fetchByName name)
                      >>= execute . flip bind () >>= fetch

getByNumber :: IConnection conn => conn -> String -> IO (Maybe Player)
getByNumber conn numStr = prepare conn (relationalQuery $ fetchByNumber name)
                          >>= execute . flip bind () >>= fetch

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
