module DataSource where

import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

dbName :: String
dbName = "bbstats-test"

schemaName :: String
schemaName = "public"

connect :: IO Connection
connect = connectPostgreSQL $ "dbname=" ++ dbName

gameScoreCsvFileName :: String
gameScoreCsvFileName = "score.csv"

battingResultCsvFileName :: String
battingResultCsvFileName = "batting.csv"

pitchingResultCsvFileName :: String
pitchingResultCsvFileName = "pitching.csv"

atBatCsvFileName :: String
atBatCsvFileName = "atbat.csv"
