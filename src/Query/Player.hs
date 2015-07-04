{-# LANGUAGE FlexibleContexts, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Query.Player
    ( find
    , findAll
    , findByNumber
    , findByName
    , persist
    ) where

import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.Player as M

-- select

find :: Int32 -> Relation () M.Player
find pid = relation $ do
  q <- query M.mstPlayer
  wheres $ q ! M.id' .=. value pid
  return q

findAll :: Relation () Player
findAll = relation $ query M.mstPlayer

findByNumber :: String -> Relation () Player
findByNumber numStr = relation $ do
  q <- query mstPlayer
  wheres $ q ! M.uniformNumber' .=. value (Just numStr)
  return q

findByName :: String -> Relation () M.Player
findByName name = relation $ do
  q <- query M.mstPlayer
  wheres $ q ! M.playerName' .=. value name
  return q

-- insert

piPlayerP :: Pi Player PlayerP
piPlayerP = PlayerP
            |$| playerName'
            |*| uniformNumber'
            |*| tempUniformNumber'

persist :: PlayerP -> InsertQuery ()
persist pm = typedInsertQuery tableOfMstPlayer piPlayerP $
            relation . return $ PlayerP
            |$| value (pPlayerName pm)
            |*| value (pUniformNumber pm)
            |*| value (pTempUniformNumber pm)
