{-# LANGUAGE FlexibleContexts #-}

module Query.BattingResult
    ( findListByGameId
    , findListByPlayerId
    , persist
    ) where

import           Database.Relational.Query
import           GHC.Int (Int32)

import           Model.BattingResult

-- select

findListByGameId :: Int32 -> Relation () BattingResult
findListByGameId gid = relation $ do
  q <- query tblBattingResult
  wheres $ q ! gameId' .=. value gid
  return q

findListByPlayerId :: Int32 -> Relation () BattingResult
findListByPlayerId pid = relation $ do
  q <- query tblBattingResult
  wheres $ q ! playerId' .=. value pid
  return q

-- insert

piBattingResultP :: Pi BattingResult BattingResultP
piBattingResultP = BattingResultP
                   |$| gameId'
                   |*| playerId'
                   |*| battingOrder'
                   |*| appearanceOrder'
                   |*| positions'
                   |*| runs'
                   |*| stolenBases'
                   |*| errors'

persist :: BattingResultP -> InsertQuery ()
persist b = typedInsertQuery tableOfTblBattingResult piBattingResultP $
            relation . return $ BattingResultP
            |$| value (pGameId b)
            |*| value (pPlayerId b)
            |*| value (pBattingOrder b)
            |*| value (pAppearanceOrder b)
            |*| value (pPositions b)
            |*| value (pRuns b)
            |*| value (pStolenBases b)
            |*| value (pErrors b)
