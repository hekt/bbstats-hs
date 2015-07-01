module Controller.ScoreController where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BS
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)
import           Database.Relational.Query (query)
import           GHC.Int (Int32)

import           DataSource (connect)
import qualified Handler.AtBat as AtBat
import qualified Handler.GameScore as Score
import qualified Handler.BattingResult as Batting
import qualified Handler.PitchingResult as Pitching

listAction :: IConnection conn => conn -> IO ()
listAction conn = do
  scores <- Score.getAll conn
  mapM_ (BS.putStrLn . encodePretty) scores

detailAction :: IConnection conn => conn -> Int32 -> IO ()
detailAction conn gid = do
  atbats    <- AtBat.getListByGameId conn gid
  score     <- Score.get conn gid
  battings  <- Batting.getListWithPlayerByGameId conn gid
  pitchings <- Pitching.getListWithPlayerByGameId conn gid

  BS.putStrLn . encodePretty $ object
        [ "score"     .= toJSON score
        , "atbats"    .= toJSON atbats
        , "battings"  .= toJSON battings
        , "pitchings" .= toJSON pitchings
        ]
