{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Model.Player where

import           Control.Monad
import           Data.Aeson
import           Database.HDBC.Query.TH
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Database.Record.TH (derivingShow)

import           DataSource (connect)

data PlayerP = PlayerP
               { pPlayerName        :: String
               , pUniformNumber     :: Maybe String
               , pTempUniformNumber :: Maybe String
               } deriving (Show)

$(defineTableFromDB connect
  driverPostgreSQL "public" "mst_player" [derivingShow])
$(makeRecordPersistableDefault ''PlayerP)

type Player = MstPlayer

instance ToJSON Player where
  toJSON m = object
             [ "player_name"         .= playerName m
             , "uniform_number"      .= uniformNumber m
             , "temp_uniform_number" .= tempUniformNumber m
             ]

instance ToJSON PlayerP where
  toJSON m = object
             [ "player_name"         .= pPlayerName m
             , "uniform_number"      .= pUniformNumber m
             , "temp_uniform_number" .= pTempUniformNumber m
             ]
instance FromJSON PlayerP where
  parseJSON (Object v) = PlayerP
                         <$> v .: "player_name"
                         <*> v .: "uniform_number"
                         <*> v .: "temp_uniform_number"
  parseJSON _          = mzero

tableName :: String
tableName = "mst_player"
