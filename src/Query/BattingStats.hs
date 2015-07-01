{-# LANGUAGE FlexibleContexts #-}

module Query.BattingStats
    ( find
    , findAll
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.BattingStats
    
find :: Int32 -> Relation () BattingStats
find pid = relation $ do
  q <- query viewBattingStats
  wheres $ q ! playerId' .=. value (Just pid)
  return q

findAll :: Relation () BattingStats
findAll = relation $ query viewBattingStats
