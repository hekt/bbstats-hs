{-# LANGUAGE FlexibleContexts #-}

module Query.BattingResult
    ( findListByGameId
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.BattingResult

findListByGameId :: Int32 -> Relation () BattingResult
findListByGameId gid = relation $ do
  q <- query tblBattingResult
  wheres $ q ! gameId' .=. value gid
  return q
