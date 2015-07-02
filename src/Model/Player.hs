{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.Player where

import           Data.Aeson
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)

import           DataSource (connect)

$(defineTableFromDB connect
  driverPostgreSQL "public" "mst_player" [derivingShow])

type Player = MstPlayer

instance ToJSON Player where
    toJSON m = object
               [ "player_name"         .= playerName m
               , "uniform_number"      .= uniformNumber m
               , "temp_uniform_number" .= tempUniformNumber m
               ]