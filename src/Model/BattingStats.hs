{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.BattingStats where

import           Data.Aeson
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)

import DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "public" "view_batting_stats" [derivingShow])

type BattingStats = ViewBattingStats

instance ToJSON BattingStats where
    toJSON m =
        object
        [ "player_id"         .= playerId m
        , "plate_appearances" .= plateAppearances m
        , "at_bats"           .= atBats m
        , "at_bats_risp"      .= atBatsRisp m
        , "rbi"               .= rbi m
        , "hits"              .= hits m
        , "hits_risp"         .= hitsRisp m
        , "walks"             .= walks m
        , "home_runs"         .= homeRuns m
        , "runs"              .= runs m
        , "stolen_bases"      .= stolenBases m
        ]
