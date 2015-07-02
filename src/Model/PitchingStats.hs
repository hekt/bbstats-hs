{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.PitchingStats where

import           Data.Aeson
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)

import DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "public" "view_pitching_stats" [derivingShow])

type PitchingStats = ViewPitchingStats

instance ToJSON PitchingStats where
    toJSON m =
        object
        [ "player_id"           .= playerId m
        , "player_name"         .= playerName m
        , "uniform_number"      .= uniformNumber m
        , "temp_uniform_number" .= tempUniformNumber m
        , "games"               .= games m
        , "outs"                .= outs m
        , "batters_faced"       .= battersFaced m
        , "runs"                .= runs m
        , "earned_runs"         .= earnedRuns m
        , "strike_outs"         .= strikeOuts m
        , "walks"               .= walks m
        , "hits"                .= hits m
        , "home_runs"           .= homeRuns m
        , "errors"              .= errors m
        , "wins"                .= wins m
        , "loses"               .= loses m
        , "holds"               .= holds m
        , "saves"               .= saves m
        ]
