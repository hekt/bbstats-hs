{-# LANGUAGE FlexibleContexts #-}

module Query.GameScore
    ( find
    , findAll
    , findByDate
    ) where

import           Database.Relational.Query hiding (id')
import           Data.Time.Calendar (Day)
import           GHC.Int (Int32)

import           Model.GameScore

find :: Int32 -> Relation () GameScore
find gid = relation $ do
  q <- query tblGameScore
  wheres $ q ! id' .=. value gid
  return q

findAll :: Relation () GameScore
findAll = relation $  do
  q <- query tblGameScore
  desc $ q ! gameDate'
  return q

findByDate :: Day -> Relation () GameScore
findByDate day = relation $ do
  q <- query tblGameScore
  wheres $ q ! gameDate' .=. value day
  return q
