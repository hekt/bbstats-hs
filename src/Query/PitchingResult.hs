{-# LANGUAGE FlexibleContexts #-}

module Query.PitchingResult
    ( findListByGameId
    , findListByPlayerId
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.PitchingResult

findListByGameId :: Int32 -> Relation () PitchingResult
findListByGameId gid = relation $ do
  q <- query tblPitchingResult
  wheres $ q ! gameId' .=. value gid
  return q

findListByPlayerId :: Int32 -> Relation () PitchingResult
findListByPlayerId pid = relation $ do
  q <- query tblPitchingResult
  wheres $ q ! playerId' .=. value pid
  return q
    
