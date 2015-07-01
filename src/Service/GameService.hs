{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Service.GameService where

import           Data.Aeson
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.Relational.Query (query)
import           GHC.Int (Int32)

import           DataSource (connect)
import qualified Handler.AtBat as AtBat
import qualified Handler.GameScore as Score
import qualified Handler.BattingResult as Batting
import qualified Handler.PitchingResult as Pitching

gameSummaryList :: IO Value
gameSummaryList = handleSqlError' $ withConnectionIO connect $ \conn -> do
  scores <- Score.getAll conn
  return $ toJSON scores

gameDetail :: Int32 -> IO Value
gameDetail gid = handleSqlError' $ withConnectionIO connect $ \conn -> do
  atbats    <- AtBat.getListByGameId conn gid
  score     <- Score.get conn gid
  battings  <- Batting.getListWithPlayerByGameId conn gid
  pitchings <- Pitching.getListWithPlayerByGameId conn gid

  return $ object [ "score"     .= toJSON score
                  , "atbats"    .= toJSON atbats
                  , "battings"  .= toJSON battings
                  , "pitchings" .= toJSON pitchings
                  ]
