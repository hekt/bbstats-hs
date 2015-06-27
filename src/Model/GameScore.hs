{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.GameScore where

import           Data.Aeson
import           Data.List.Split (splitOn)
import           Data.Time.Calendar (Day, toModifiedJulianDay)
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)
import           GHC.Int (Int16)

import           DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_game_score" [derivingShow])

type GameScore = TblGameScore

instance ToJSON GameScore where
    toJSON m =
        object
        [ "game_date"             .= toModifiedJulianDay (gameDate m)
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

gameResultKindWin  :: Int16
gameResultKindLose :: Int16
gameResultKindDraw :: Int16
gameResultKindWin   = 0
gameResultKindLose  = 1
gameResultKindDraw  = 2

toGameResultKindString :: Int16 -> String
toGameResultKindString 0 = "WIN"
toGameResultKindString 1 = "LOSE"
toGameResultKindString 2 = "DRAW"

attackTurnKindTop    :: Int16
attackTurnKindBottom :: Int16
attackTurnKindTop     = 0
attackTurnKindBottom  = 1

toAttackTurnString :: Int16 -> String
toAttackTurnString 0 = "TOP"
toAttackTurnString 1 = "BOTTOM"

toRunsList :: String -> [Int]
toRunsList = read
