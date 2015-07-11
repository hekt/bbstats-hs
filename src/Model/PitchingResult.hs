{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.PitchingResult where

import           Data.Aeson
import qualified Data.Csv as Csv
import           Database.HDBC.Query.TH
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)
import           Database.Relational.Query
import           GHC.Int (Int16, Int32)

import           DataSource (connect)
import           Model.Util

data PitchingResultP = PitchingResultP
                       { pGameId          :: Int32
                       , pPlayerId        :: Int32
                       , pAppearanceOrder :: Int16
                       , pOuts            :: Int16
                       , pBattersFaced    :: Int16
                       , pRuns            :: Int16
                       , pEarnedRuns      :: Int16
                       , pStrikeOuts      :: Int16
                       , pWalks           :: Int16
                       , pHits            :: Int16
                       , pHomeRuns        :: Int16
                       , pErrors          :: Int16
                       , pDecision        :: Int16
                       } deriving (Show)

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_pitching_result" [derivingShow])
$(makeRecordPersistableDefault ''PitchingResultP)

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

instance ToJSON PitchingResultP where
  toJSON m =
    object
    [ "game_id"          .= pGameId m
    , "player_id"        .= pPlayerId m
    , "appearance_order" .= pAppearanceOrder m
    , "outs"             .= pOuts m
    , "batters_faced"    .= pBattersFaced m
    , "runs"             .= pRuns m
    , "earned_runs"      .= pEarnedRuns m
    , "strike_outs"      .= pStrikeOuts m
    , "walks"            .= pWalks m
    , "hits"             .= pHits m
    , "home_runs"        .= pHomeRuns m
    , "errors"           .= pErrors m
    , "decision"         .= toPitchingDecisionString (pDecision m)
    ]
instance FromJSON PitchingResultP where
  parseJSON (Object v) = PitchingResultP
                         <$> v .: "game_id"
                         <*> v .: "player_id"
                         <*> v .: "appearance_order"
                         <*> v .: "outs"
                         <*> v .: "batters_faced"
                         <*> v .: "runs"
                         <*> v .: "earned_runs"
                         <*> v .: "strike_outs"
                         <*> v .: "walks"
                         <*> v .: "hits"
                         <*> v .: "home_runs"
                         <*> v .: "errors"
                         <*> (toPitchingDecision <$> v .: "decision")

instance Csv.FromNamedRecord PitchingResultP where
  parseNamedRecord m = PitchingResultP
                       <$> m Csv..: "game_id"
                       <*> m Csv..: "player_id"
                       <*> m Csv..: "appearance_order"
                       <*> m Csv..: "outs"
                       <*> m Csv..: "batters_faced"
                       <*> m Csv..: "runs"
                       <*> m Csv..: "earned_runs"
                       <*> m Csv..: "strike_outs"
                       <*> m Csv..: "walks"
                       <*> m Csv..: "hits"
                       <*> m Csv..: "home_runs"
                       <*> m Csv..: "errors"
                       <*> m Csv..: "decision"

piPitchingResultP :: Pi PitchingResult PitchingResultP
piPitchingResultP = PitchingResultP
                    |$| gameId'
                    |*| playerId'
                    |*| appearanceOrder'
                    |*| outs'
                    |*| battersFaced'
                    |*| runs'
                    |*| earnedRuns'
                    |*| strikeOuts'
                    |*| walks'
                    |*| hits'
                    |*| homeRuns'
                    |*| errors'
                    |*| decision'

tableName :: String
tableName = "tbl_pitching_result"

pitchingDecisionWin  :: Int16
pitchingDecisionLose :: Int16
pitchingDecisionHold :: Int16
pitchingDecisionSave :: Int16
pitchingDecisionWin   = 0
pitchingDecisionLose  = 1
pitchingDecisionHold  = 2
pitchingDecisionSave  = 3

toPitchingDecision :: String -> Int16
toPitchingDecision "WIN"  = 0
toPitchingDecision "LOSE" = 1
toPitchingDecision "HOLD" = 2
toPitchingDecision "SAVE" = 3

toPitchingDecisionString :: Int16 -> String
toPitchingDecisionString 0 = "WIN"
toPitchingDecisionString 1 = "LOSE"
toPitchingDecisionString 2 = "HOLD"
toPitchingDecisionString 3 = "SAVE"
