{-# LANGUAGE FlexibleContexts #-}

module Handler.Util where

import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BS
import           Data.Csv (FromNamedRecord, decodeByName)
import           Data.List (intercalate)
import qualified Data.Vector as V
import qualified Database.HDBC.Record.Query as Q
import           Database.HDBC.Record.Statement (bind, execute)
import           Database.HDBC.Types (IConnection)
import           Database.HDBC.SqlValue (SqlValue)
import           Database.Record.FromSql (FromSql)
import           Database.Relational.Query

import           DataSource (schemaName)

fetch :: (IConnection conn, FromSql SqlValue a) =>
         conn -> Relation () a -> IO (Maybe a)
fetch conn q = Q.prepare conn (relationalQuery q)
               >>= execute . flip bind () >>= Q.fetch

fetchAll' :: (IConnection conn, FromSql SqlValue a) =>
         conn -> Relation () a -> IO [a]
fetchAll' conn q = Q.prepare conn (relationalQuery q)
                  >>= execute . flip bind () >>= Q.fetchAll'

putAllFromCSVWithPutAction :: FromNamedRecord r => FilePath
                           -> (r -> IO Integer) -> IO (Either String Integer)
putAllFromCSVWithPutAction path putAction = runExceptT $ do
  csvBs        <- liftIO $ BS.readFile path
  (_, records) <- ExceptT . return $ decodeByName csvBs
  affecteds    <- liftIO $ V.forM records $ putAction
  return $ V.sum affecteds

refleshSql :: String -> String
refleshSql tableName = concat [ "REFRESH MATERIALIZED VIEW "
                              , schemaName, ".", tableName, ";" ]

copySql :: String -> [String] -> FilePath -> String
copySql table columns absPath =
  let to = concat [ table, "(", intercalate "," columns, ")"]
      from = '\'': absPath ++ "'"
  in unwords [ "COPY", to, "FROM", from
             , "(DELIMITER ',', FORMAT csv, HEADER true);" ]
