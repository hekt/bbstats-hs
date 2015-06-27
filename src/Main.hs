module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.Map as Map
import           Database.Relational.Query (relationalQuery)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Record.Query (runQuery)
import           System.Environment (getArgs)

import           DataSource (connect)
import qualified Handler.Player as Player
import qualified Handler.GameScore as Game
-- import Query.AtBat as AtBat (fetchListByGameId)
-- import Query.BattingResult as Batting (fetchListByGameId)
-- import Query.PitchingResult as Pitching (fetchListByGameId)

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
  rs <- Game.getAll conn
  mapM_ (BS.putStrLn . encodePretty) rs
