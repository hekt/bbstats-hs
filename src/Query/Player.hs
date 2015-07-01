{-# LANGUAGE FlexibleContexts #-}

module Query.Player
    ( findAll
    , findByNumber
    , findByName
    ) where

import Database.Relational.Query

import qualified Model.Player as M

findAll :: Relation () M.Player
findAll = relation $ query M.mstPlayer

findByNumber :: String -> Relation () M.Player
findByNumber numStr = relation $ do
  q <- query M.mstPlayer
  wheres $ q ! M.uniformNumber' .=. value (Just numStr)
  return q

findByName :: String -> Relation () M.Player
findByName name = relation $ do
  q <- query M.mstPlayer
  wheres $ q ! M.playerName' .=. value name
  return q
