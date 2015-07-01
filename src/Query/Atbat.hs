{-# LANGUAGE FlexibleContexts #-}

module Query.AtBat
    ( findListByGameId
    , findListByPlayerId
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.AtBat

findListByGameId :: Int32 -> Relation () AtBat
findListByGameId gid = relation $ do
  q <- query tblAtBat
  wheres $ q ! gameId' .=. value gid
  return q

findListByPlayerId :: Int32 -> Relation () AtBat
findListByPlayerId pid = relation $ do
  q <- query tblAtBat
  wheres $ q ! playerId' .=. value pid
  return q
