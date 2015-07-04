{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.GameScore where

import           Control.Monad
import           Data.Aeson
import           Data.List.Split (splitOn)
import           Data.Time.Calendar (Day)
import           Database.HDBC.Query.TH
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)
import           GHC.Int (Int16)
import           Text.Read (readMaybe)

import           DataSource (connect)
import           Model.Util

data GameScoreP = GameScoreP
               { pGameDate            :: Day
               , pGameNumber          :: Int16
               , pGameResult          :: Int16
               , pGround              :: String
               , pAttackTurn          :: Int16
               , pRuns                :: String
               , pTotalRuns           :: Int16
               , pTotalHits           :: Int16
               , pTotalErrors         :: Int16
               , pOpponentName        :: Maybe String
               , pOpponentRuns        :: Maybe String
               , pOpponentTotalRuns   :: Maybe Int16
               , pOpponentTotalHits   :: Maybe Int16
               , pOpponentTotalErrors :: Maybe Int16
               } deriving (Show)

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_game_score" [derivingShow])
$(makeRecordPersistableDefault ''GameScoreP)

type GameScore = TblGameScore

instance ToJSON GameScore where
  toJSON m =
    object
    [ "game_date"             .= gameDate m
    , "game_number"           .= gameNumber m
    , "game_result"           .= toGameResultKindString (gameResult m)
    , "ground"                .= ground m
    , "attack_turn"           .= toAttackTurnString (attackTurn m)
    , "runs"                  .= toRunsList (runs m)
    , "total_runs"            .= totalRuns m
    , "total_hits"            .= totalHits m
    , "total_errors"          .= totalErrors m
    , "opponent_name"         .= opponentName m
    , "opponent_runs"         .= (opponentRuns m >>= return . toRunsList)
    , "opponent_total_runs"   .= opponentTotalRuns m
    , "opponent_total_hits"   .= opponentTotalHits m
    , "opponent_total_errors" .= opponentTotalErrors m
    ]

instance ToJSON GameScoreP where
  toJSON m =
    object
    [ "game_date"             .= pGameDate m
    , "game_number"           .= pGameNumber m
    , "game_result"           .= toGameResultKindString (pGameResult m)
    , "ground"                .= pGround m
    , "attack_turn"           .= toAttackTurnString (pAttackTurn m)
    , "runs"                  .= toRunsList (pRuns m)
    , "total_runs"            .= pTotalRuns m
    , "total_hits"            .= pTotalHits m
    , "total_errors"          .= pTotalErrors m
    , "opponent_name"         .= pOpponentName m
    , "opponent_runs"         .= (pOpponentRuns m >>= return . toRunsList)
    , "opponent_total_runs"   .= pOpponentTotalRuns m
    , "opponent_total_hits"   .= pOpponentTotalHits m
    , "opponent_total_errors" .= pOpponentTotalErrors m
    ]
instance FromJSON GameScoreP where
  parseJSON (Object v) = GameScoreP
                         <$> v .: "game_date"
                         <*> v .: "game_number"
                         <*> v .: "game_result"
                         <*> v .: "ground"
                         <*> v .: "attack_turn"
                         <*> asString v "runs"
                         <*> v .: "total_runs"
                         <*> v .: "total_hits"
                         <*> v .: "total_errors"
                         <*> v .: "opponent_name"
                         <*> asString v "opponent_runs"
                         <*> v .: "opponent_total_runs"
                         <*> v .: "opponent_total_hits"
                         <*> v .: "opponent_total_errors"
  parseJSON _          = mzero

tableName :: String
tableName = "tbl_game_score"

gameResultKindWin  :: Int16
gameResultKindLose :: Int16
gameResultKindDraw :: Int16
gameResultKindWin   = 0
gameResultKindLose  = 1
gameResultKindDraw  = 2

toGameResultKindString :: Int16 -> Maybe String
toGameResultKindString 0 = Just "WIN"
toGameResultKindString 1 = Just "LOSE"
toGameResultKindString 2 = Just "DRAW"
toGameResultKindString _ = Nothing

attackTurnKindTop    :: Int16
attackTurnKindBottom :: Int16
attackTurnKindTop     = 0
attackTurnKindBottom  = 1

toAttackTurnString :: Int16 -> Maybe String
toAttackTurnString 0 = Just "TOP"
toAttackTurnString 1 = Just "BOTTOM"
toAttackTurnString _ = Nothing

toRunsList :: String -> Maybe [Int]
toRunsList = readMaybe
