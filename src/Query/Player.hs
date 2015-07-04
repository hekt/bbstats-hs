{-# LANGUAGE FlexibleContexts #-}

module Query.Player
    ( find
    , findAll
    , findByNumber
    , findByName
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.Player as M

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
