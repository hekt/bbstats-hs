{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.AtBat where

import           Data.Aeson
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)

import           DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_at_bat" [derivingShow])

type AtBat = TblAtBat

instance ToJSON AtBat where
    toJSON m =
        object
        [ "game_id"          .= gameId m
        , "player_id"        .= playerId m
        , "at_bat_number"    .= atBatNumber m
        , "inning"           .= inning m
        , "rbi"              .= rbi m
        , "out_count"        .= outCount m
        , "result_text"      .= resultText m
        , "result_kind"      .= resultKind m
        , "is_risp"          .= isRisp m
        , "is_counts_at_bat" .= isCountsAtBat m
        ]
