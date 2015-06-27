{-# LANGUAGE FlexibleContexts #-}

module Query.AtBat
    ( fetchListByGameId
    ) where

import Database.Relational.Query
import GHC.Int (Int32)

import qualified Model.Database.TblAtBat as A

fetchListByGameId :: Int32 -> Relation () A.TblAtBat
fetchListByGameId gameId = relation $ do
  q <- query A.tblAtBat
  wheres $ q ! A.gameId' .=. value gameId
  return $ make q

make q = A.TblAtBat
          |$| q ! A.id'
          |*| q ! A.gameId'
          |*| q ! A.playerId'
          |*| q ! A.atBatNumber'
          |*| q ! A.inning'
          |*| q ! A.rbi'
          |*| q ! A.outCount'
          |*| q ! A.resultText'
          |*| q ! A.resultKind'
          |*| q ! A.isRisp'
          |*| q ! A.isCountsAtBat'
          |*| q ! A.createdAt'
          |*| q ! A.updatedAt'
