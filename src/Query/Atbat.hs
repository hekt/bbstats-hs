{-# LANGUAGE FlexibleContexts #-}

module Query.AtBat
    ( findListByGameId
    , findListByPlayerId
    , persist
    ) where

import           Database.Relational.Query
import           GHC.Int (Int16, Int32)

import           Model.AtBat

-- select

findListByGameId :: Int32 -> Relation () AtBat
findListByGameId gid = relation $ do
  q <- query tblAtBat
  wheres $ q ! gameId' .=. value gid
  return q

findListByPlayerId :: Int32 -> Relation () AtBat
findListByPlayerId pid = relation $ do
  q <- query tblAtBat
  wheres $ q ! playerId' .=. value pid
  return q

-- insert

persist :: AtBatP -> InsertQuery ()
persist a = typedInsertQuery tableOfTblAtBat piAtBatP $
            relation . return $ AtBatP
            |$| value (pGameId a)
            |*| value (pPlayerId a)
            |*| value (pAtBatNumber a)
            |*| value (pInning a)
            |*| value (pRbi a)
            |*| value (pOutCount a)
            |*| value (pResultText a)
            |*| value (pResultKind a)
            |*| value (pIsRisp a)
            |*| value (pIsCountsAtBat a)
