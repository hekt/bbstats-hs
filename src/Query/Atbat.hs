{-# LANGUAGE FlexibleContexts #-}

module Query.Atbat
    ( fetchListByGameId
    ) where

import Database.Relational.Query
import GHC.Int (Int32)

import qualified Models.Atbat as A

fetchListByGameId :: Int32 -> Relation () A.Atbat
fetchListByGameId gameId = relation $ do
  q <- query A.atbat
  wheres $ q ! A.gameId' .=. value gameId
  return $ make q

make q = A.Atbat
          |$| q ! A.id'
          |*| q ! A.gameId'
          |*| q ! A.playerId'
          |*| q ! A.atbatNumber'
          |*| q ! A.inning'
          |*| q ! A.rbi'
          |*| q ! A.outCount'
          |*| q ! A.resultText'
          |*| q ! A.resultKind'
          |*| q ! A.isRisp'
          |*| q ! A.isCountsAtbat'
          |*| q ! A.createdAt'
          |*| q ! A.updatedAt'
