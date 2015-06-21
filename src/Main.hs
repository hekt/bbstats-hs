module Main where

import Control.Monad
import Database.Relational.Query (relationalQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery)
import System.Environment (getArgs)

import DataSource (connect)
import Query.Player as Player (fetchByNumber, fetchByName)
import Query.GameScore as Game (fetchByDate , fetchLostGameList)
import Query.Atbat as Atbat (fetchListByGameId)
import Query.BattingResult as Batting (fetchListByGameId)
import Query.PitchingResult as Pitching (fetchListByGameId)

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
  (arg:_) <- getArgs
  rows <- runQuery conn (relationalQuery $ Player.fetchByNumber arg) ()
  mapM_ print rows
