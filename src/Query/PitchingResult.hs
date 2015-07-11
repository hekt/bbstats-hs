{-# LANGUAGE FlexibleContexts #-}

module Query.PitchingResult
    ( findListByGameId
    , findListByPlayerId
    , persist
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
    

-- insert

persist :: PitchingResultP -> InsertQuery ()
persist p = typedInsertQuery tableOfTblPitchingResult piPitchingResultP $
            relation . return $ PitchingResultP
            |$| value (pGameId p)
            |*| value (pPlayerId p)
            |*| value (pAppearanceOrder p)
            |*| value (pOuts p)
            |*| value (pBattersFaced p)
            |*| value (pRuns p)
            |*| value (pEarnedRuns p)
            |*| value (pStrikeOuts p)
            |*| value (pWalks p)
            |*| value (pHits p)
            |*| value (pHomeRuns p)
            |*| value (pErrors p)
            |*| value (pDecision p)
