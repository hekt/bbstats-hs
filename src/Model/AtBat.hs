{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.AtBat where

import           Data.Aeson
import           Database.HDBC.Query.TH
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)
import           GHC.Int (Int16, Int32)

import           DataSource (connect)

data AtBatP = AtBatP
              { pGameId        :: Int32
              , pPlayerId      :: Int32
              , pAtBatNumber   :: Int16
              , pInning        :: Int16
              , pRbi           :: Int16
              , pOutCount      :: Int16
              , pResultText    :: String
              , pResultKind    :: String
              , pIsRisp        :: Bool
              , pIsCountsAtBat :: Bool
              }

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_at_bat" [derivingShow])
$(makeRecordPersistableDefault ''AtBatP)

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

instance ToJSON AtBatP where
  toJSON m =
    object
    [ "game_id"          .= pGameId m
    , "player_id"        .= pPlayerId m
    , "at_bat_number"    .= pAtBatNumber m
    , "inning"           .= pInning m
    , "rbi"              .= pRbi m
    , "out_count"        .= pOutCount m
    , "result_text"      .= pResultText m
    , "result_kind"      .= pResultKind m
    , "is_risp"          .= pIsRisp m
    , "is_counts_at_bat" .= pIsCountsAtBat m
    ]
instance FromJSON AtBatP where
  parseJSON (Object v) = AtBatP
                         <$> v .: "game_id"
                         <*> v .: "player_id"
                         <*> v .: "at_bat_number"
                         <*> v .: "inning"
                         <*> v .: "rbi"
                         <*> v .: "out_count"
                         <*> v .: "result_text"
                         <*> v .: "result_kind"
                         <*> v .: "is_risp"
                         <*> v .: "is_counts_at_bat"

tableName :: String
tableName = "tbl_at_bat"

insertColumnNames :: [String]
insertColumnNames = [ "game_id"
                    , "player_id"
                    , "at_bat_number"
                    , "inning"
                    , "rbi"
                    , "out_count"
                    , "result_text"
                    , "result_kind"
                    , "is_risp"
                    , "is_counts_at_bat"
                    ]
