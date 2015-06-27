{-# LANGUAGE FlexibleContexts #-}

module Query.GameScore
    ( fetchAll
    , fetchByDate
    ) where

import Database.Relational.Query
import Data.Time.Calendar (Day)

import qualified Model.GameScore as M

fetchAll :: Relation () M.GameScore
fetchAll = relation $  do
  q <- query M.tblGameScore
  desc $ q ! M.gameDate'
  return $ make q

fetchByDate :: Day -> Relation () M.GameScore
fetchByDate day = relation $ do
  q <- query M.tblGameScore
  wheres $ q ! M.gameDate' .=. value day
  return $ make q

make q = M.TblGameScore
       |$| q ! M.id'
       |*| q ! M.gameDate'
       |*| q ! M.gameNumber'
       |*| q ! M.gameResult'
       |*| q ! M.ground'
       |*| q ! M.attackTurn'
       |*| q ! M.runs'
       |*| q ! M.totalRuns'
       |*| q ! M.totalHits'
       |*| q ! M.totalErrors'
       |*| q ! M.opponentName'
       |*| q ! M.opponentRuns'
       |*| q ! M.opponentTotalRuns'
       |*| q ! M.opponentTotalHits'
       |*| q ! M.opponentTotalErrors'
       |*| q ! M.createdAt'
       |*| q ! M.updatedAt'
