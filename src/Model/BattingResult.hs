{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.BattingResult where

import           Data.Aeson
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)
import           Safe (readMay)

import           DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_batting_result" [derivingShow])

type BattingResult = TblBattingResult

instance ToJSON BattingResult where
    toJSON m =
        object
        [ "game_id"          .= gameId m
        , "player_id"        .= playerId m
        , "batting_order"    .= battingOrder m
        , "appearance_order" .= appearanceOrder m
        , "positions"        .= toPositionsList (positions m)
        , "runs"             .= runs m
        , "stolen_bases"     .= stolenBases m
        , "errors"           .= errors m
        ]

toPositionsList :: String -> Maybe [Int]
toPositionsList =  readMay
