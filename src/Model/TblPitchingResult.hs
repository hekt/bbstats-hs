{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Model.Database.TblPitchingResult where

import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow)
import GHC.Int (Int16)

import DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "public" "tbl_pitching_result" [derivingShow])

pitcherDecisionKindWin :: GHC.Int.Int16
pitcherDecisionKindWin = 0

pitcherDecisionKindLose:: GHC.Int.Int16
pitcherDecisionKindLose = 1

pitcherDecisionKindHold :: GHC.Int.Int16
pitcherDecisionKindHold = 2

pitcherDecisionKindSave :: GHC.Int.Int16
pitcherDecisionKindSave = 3
