{-# LANGUAGE FlexibleContexts #-}

module Query.BattingResult
    ( fetchListByGameId
    ) where

import Database.Relational.Query
import GHC.Int (Int32)

import qualified Models.BattingResult as B

fetchListByGameId :: Int32 -> Relation () B.BattingResult
fetchListByGameId gameId = relation $ do
  q <- query B.battingResult
  wheres $ q ! B.gameId' .=. value gameId
  return $ make q

make q = B.BattingResult
        |$| q ! B.id'
        |*| q ! B.gameId'
        |*| q ! B.playerId'
        |*| q ! B.battingOrder'
        |*| q ! B.appearanceOrder'
        |*| q ! B.positions'
        |*| q ! B.runs'
        |*| q ! B.stolenBases'
        |*| q ! B.errors'
        |*| q ! B.createdAt'
        |*| q ! B.updatedAt'
