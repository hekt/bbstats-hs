{-# LANGUAGE FlexibleContexts #-}

module Query.PitchingResult
    ( fetchListByGameId
    ) where

import Database.Relational.Query
import GHC.Int (Int32)

import qualified Models.PitchingResult as P

fetchListByGameId :: GHC.Int.Int32 -> Relation () P.PitchingResult
fetchListByGameId gameId = relation $ do
  q <- query P.pitchingResult
  wheres $ q ! P.gameId' .=. value gameId
  return $ make q

make q = P.PitchingResult
        |$| q ! P.id'
        |*| q ! P.gameId'
        |*| q ! P.playerId'
        |*| q ! P.appearanceOrder'
        |*| q ! P.outs'
        |*| q ! P.battersFaced'
        |*| q ! P.runs'
        |*| q ! P.earnedRuns'
        |*| q ! P.strikeouts'
        |*| q ! P.walks'
        |*| q ! P.hits'
        |*| q ! P.homeruns'
        |*| q ! P.errors'
        |*| q ! P.decision'
        |*| q ! P.createdAt'
        |*| q ! P.updatedAt'
