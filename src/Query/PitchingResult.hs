{-# LANGUAGE FlexibleContexts #-}

module Query.PitchingResult
    ( fetchListByGameId
    ) where

import Database.Relational.Query
import GHC.Int (Int32)

import qualified Model.Database.TblPitchingResult as P

fetchListByGameId :: GHC.Int.Int32 -> Relation () P.TblPitchingResult
fetchListByGameId gameId = relation $ do
  q <- query P.tblPitchingResult
  wheres $ q ! P.gameId' .=. value gameId
  return $ make q

make q = P.TblPitchingResult
        |$| q ! P.id'
        |*| q ! P.gameId'
        |*| q ! P.playerId'
        |*| q ! P.appearanceOrder'
        |*| q ! P.outs'
        |*| q ! P.battersFaced'
        |*| q ! P.runs'
        |*| q ! P.earnedRuns'
        |*| q ! P.strikeOuts'
        |*| q ! P.walks'
        |*| q ! P.hits'
        |*| q ! P.homeRuns'
        |*| q ! P.errors'
        |*| q ! P.decision'
        |*| q ! P.createdAt'
        |*| q ! P.updatedAt'
