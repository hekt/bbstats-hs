{-# LANGUAGE FlexibleContexts #-}

module Handler.Util where

import qualified Database.HDBC.Record.Query as Q
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Types (IConnection)
import           Database.HDBC.SqlValue (SqlValue)
import           Database.Record.FromSql (FromSql)
import           Database.Relational.Query

fetch :: (IConnection conn, FromSql SqlValue a) =>
         conn -> Relation () a -> IO (Maybe a)
fetch conn q = Q.prepare conn (relationalQuery q)
               >>= execute . flip bind () >>= Q.fetch

fetchAll' :: (IConnection conn, FromSql SqlValue a) =>
         conn -> Relation () a -> IO [a]
fetchAll' conn q = Q.prepare conn (relationalQuery q)
                  >>= execute . flip bind () >>= Q.fetchAll'

refleshSql :: String -> String
refleshSql tableName = concat [ "REFRESH MATERIALIZED VIEW "
                              , tableName, ";" ]
