{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Models.GameScore where

import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow)
import GHC.Int (Int16)

import DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "bbstats" "game_score" [derivingShow])

gameResultKindWin :: GHC.Int.Int16
gameResultKindWin = 0

gameResultKindLose :: GHC.Int.Int16
gameResultKindLose = 1

gameResultKindDraw :: GHC.Int.Int16
gameResultKindDraw = 2

attackTurnKindTop :: GHC.Int.Int16
attackTurnKindTop = 0

attackTurnKindBottom :: GHC.Int.Int16
attackTurnKindBottom = 1
