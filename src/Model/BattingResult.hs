{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.BattingResult where

import           Data.Aeson
import qualified Data.Csv as Csv
import           Database.HDBC.Query.TH
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)
import           Text.Read (readMaybe)
import           GHC.Int (Int16, Int32)

import           DataSource (connect)
import           Model.Util

data BattingResultP = BattingResultP
                     { pGameId          :: Int32
                     , pPlayerId        :: Int32
                     , pBattingOrder    :: Int16
                     , pAppearanceOrder :: Int16
                     , pPositions       :: String
                     , pRuns            :: Int16
                     , pStolenBases     :: Int16
                     , pErrors          :: Int16
                     } deriving (Show)

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_batting_result" [derivingShow])
$(makeRecordPersistableDefault ''BattingResultP)

type BattingResult = TblBattingResult

instance ToJSON BattingResult where
  toJSON m = object [ "game_id"          .= gameId m
                    , "player_id"        .= playerId m
                    , "batting_order"    .= battingOrder m
                    , "appearance_order" .= appearanceOrder m
                    , "positions"        .= toPositionsList (positions m)
                    , "runs"             .= runs m
                    , "stolen_bases"     .= stolenBases m
                    , "errors"           .= errors m
                    ]

instance ToJSON BattingResultP where
  toJSON m = object [ "game_id"          .= pGameId m
                    , "player_id"        .= pPlayerId m
                    , "batting_order"    .= pBattingOrder m
                    , "appearance_order" .= pAppearanceOrder m
                    , "positions"        .= toPositionsList (pPositions m)
                    , "runs"             .= pRuns m
                    , "stolen_bases"     .= pStolenBases m
                    , "errors"           .= pErrors m
                    ]
instance FromJSON BattingResultP where
  parseJSON (Object v) = BattingResultP
                         <$> v .: "game_id"
                         <*> v .: "player_id"
                         <*> v .: "batting_order"
                         <*> v .: "appearance_order"
                         <*> asString v "positions"
                         <*> v .: "runs"
                         <*> v .: "stolen_bases"
                         <*> v .: "errors"

instance Csv.FromNamedRecord BattingResultP where
  parseNamedRecord m = BattingResultP
                       <$> m Csv..: "game_id"
                       <*> m Csv..: "player_id"
                       <*> m Csv..: "batting_order"
                       <*> m Csv..: "appearance_order"
                       <*> m Csv..: "positions"
                       <*> m Csv..: "runs"
                       <*> m Csv..: "stolen_bases"
                       <*> m Csv..: "errors"
  
tableName :: String
tableName = "tbl_batting_result"

insertColumnNames :: [String]
insertColumnNames = [ "game_id"
                    , "player_id"
                    , "batting_order"
                    , "appearance_order"
                    , "positions"
                    , "runs"
                    , "stolen_bases"
                    , "errors"
                    ]

toPositionsList :: String -> Maybe [Int]
toPositionsList = readMaybe
