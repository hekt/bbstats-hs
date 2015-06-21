{-# LANGUAGE FlexibleContexts #-}

module Query.GameScore
    ( fetchByDate
    , fetchLostGameList
    ) where

import Database.Relational.Query
import Data.Time.Calendar (Day)

import qualified Models.GameScore as G

fetchByDate :: Day -> Relation () G.GameScore
fetchByDate day = relation $ do
  q <- query G.gameScore
  wheres $ q ! G.gameDate' .=. value day
  return $ make q

fetchLostGameList :: Relation () G.GameScore
fetchLostGameList = relation $ do
  q <- query G.gameScore
  wheres $ q ! G.gameResult' .=. value G.gameResultKindLose
  return $ make q

make q = G.GameScore
       |$| q ! G.id'
       |*| q ! G.gameDate'
       |*| q ! G.gameNumber'
       |*| q ! G.gameResult'
       |*| q ! G.ground'
       |*| q ! G.attackTurn'
       |*| q ! G.runs'
       |*| q ! G.totalRuns'
       |*| q ! G.totalHits'
       |*| q ! G.totalErrors'
       |*| q ! G.opponentName'
       |*| q ! G.opponentRuns'
       |*| q ! G.opponentTotalRuns'
       |*| q ! G.opponentTotalHits'
       |*| q ! G.opponentTotalErrors'
       |*| q ! G.createdAt'
       |*| q ! G.updatedAt'
