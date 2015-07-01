{-# LANGUAGE FlexibleContexts #-}

module Query.PitchingStats
    ( find
    , findAll
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.PitchingStats

find :: Int32 -> Relation () PitchingStats
find pid = relation $ do
  q <- query viewPitchingStats
  wheres $ q ! playerId' .=. value (Just pid)
  return q

findAll :: Relation () PitchingStats
findAll = relation $ query viewPitchingStats
