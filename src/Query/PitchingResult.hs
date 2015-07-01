{-# LANGUAGE FlexibleContexts #-}

module Query.PitchingResult
    ( findListByGameId
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import qualified Model.PitchingResult as M
import qualified Model.Player as PM

findListByGameId :: Int32 -> Relation () M.PitchingResult
findListByGameId gid = relation $ do
  q <- query M.tblPitchingResult
  wheres $ q ! M.gameId' .=. value gid
  return q
