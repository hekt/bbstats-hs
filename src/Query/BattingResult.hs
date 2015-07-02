{-# LANGUAGE FlexibleContexts #-}

module Query.BattingResult
    ( findListByGameId
    , findListByPlayerId
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.BattingResult

findListByGameId :: Int32 -> Relation () BattingResult
findListByGameId gid = relation $ do
  q <- query tblBattingResult
  wheres $ q ! gameId' .=. value gid
  return q

findListByPlayerId :: Int32 -> Relation () BattingResult
findListByPlayerId pid = relation $ do
  q <- query tblBattingResult
  wheres $ q ! playerId' .=. value pid
  return q
