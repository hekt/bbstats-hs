module Service.Databse where

import           System.Cmd (system)

import           DataSource (schemaName)
import qualified Model.BattingStats (tableName)
import qualified Model.PitchingStats (tableName)

materializedViewNames :: [String]
materializedViewNames = [ BattingStats.tableName
                        , PitchingStats.tableName
                        ]

refleshMaterializedViews :: IO ()
refleshMaterializedViews = forM_ materializedViewNames (system . makeCmd)
  where makeCmd mvName = let sql = concat [ "'"
                                          , "REFRESH MATERIALIZED VIEW "
                                          , schemaName, "." , mvName, ";"
                                          , "'"
                                          ]
                         in unwords ["psql", "-d", dbName, "-c", sql]
