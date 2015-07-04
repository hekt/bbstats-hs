module Action.Command (command) where

import           System.Cmd (system)

import           DataSource (dbName, schemaName)
import qualified Model.BattingStats as BattingStats
import qualified Model.PitchingStats as PitchingStats

command :: [String] -> IO ()
command ["reflesh-stats"] = refleshStats
command _                 = help

help :: IO ()
help = putStrLn "help"

refleshStats :: IO ()
refleshStats = do
  mapM_ (system . makeRefleshViewCmd) [ BattingStats.tableName
                                      , PitchingStats.tableName ]

makeRefleshViewCmd :: String -> String
makeRefleshViewCmd name =
  let sql = concat [ "'"
                   , "REFRESH MATERIALIZED VIEW "
                   , schemaName, "." , name, ";"
                   , "'"
                   ]
  in unwords ["psql", "-d", dbName, "-c", sql]
