{-# LANGUAGE FlexibleContexts #-}

module Query.GameScore
    ( find
    , findAll
    , findByDateAndNumber
    , persist
    ) where

import           Database.Relational.Query hiding (id')
import           Data.Time.Calendar (Day)
import           GHC.Int (Int16, Int32)

import           Model.GameScore

-- select

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

findByDateAndNumber :: Day -> Int16 -> Relation () GameScore
findByDateAndNumber day num = relation $ do
  q <- query tblGameScore
  wheres $ q ! gameDate' .=. value day
  wheres $ q ! gameNumber' .=. value num
  return q

-- insert

persist :: GameScoreP -> InsertQuery ()
persist g = typedInsertQuery tableOfTblGameScore piGameScoreP $
            relation . return $ GameScoreP
            |$| value (pGameDate g)
            |*| value (pGameNumber g)
            |*| value (pGameResult g)
            |*| value (pGround g)
            |*| value (pAttackTurn g)
            |*| value (pRuns g)
            |*| value (pTotalRuns g)
            |*| value (pTotalHits g)
            |*| value (pTotalErrors g)
            |*| value (pOpponentName g)
            |*| value (pOpponentRuns g)
            |*| value (pOpponentTotalRuns g)
            |*| value (pOpponentTotalHits g)
            |*| value (pOpponentTotalErrors g)
