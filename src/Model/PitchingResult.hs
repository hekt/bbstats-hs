{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.PitchingResult where

import           Data.Aeson
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)
import           GHC.Int (Int16)

import           DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_pitching_result" [derivingShow])

type PitchingResult = TblPitchingResult

instance ToJSON PitchingResult where
    toJSON m =
        object
        [ "game_id"          .= gameId m
        , "player_id"        .= playerId m
        , "appearance_order" .= appearanceOrder m
        , "outs"             .= outs m
        , "batters_faced"    .= battersFaced m
        , "runs"             .= runs m
        , "earned_runs"      .= earnedRuns m
        , "strike_outs"      .= strikeOuts m
        , "walks"            .= walks m
        , "hits"             .= hits m
        , "home_runs"        .= homeRuns m
        , "errors"           .= errors m
        , "decision"         .= toPitchingDecisionString (decision m)
        ]

pitchingDecisionWin  :: Int16
pitchingDecisionLose :: Int16
pitchingDecisionHold :: Int16
pitchingDecisionSave :: Int16
pitchingDecisionWin   = 0
pitchingDecisionLose  = 1
pitchingDecisionHold  = 2
pitchingDecisionSave  = 3

toPitchingDecisionString :: Int16 -> String
toPitchingDecisionString 0 = "WIN"
toPitchingDecisionString 1 = "LOSE"
toPitchingDecisionString 2 = "HOLD"
toPitchingDecisionString 3 = "SAVE"
