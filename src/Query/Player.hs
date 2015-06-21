{-# LANGUAGE FlexibleContexts #-}

module Query.Player
    ( fetchByNumber
    , fetchByName
    ) where

import Database.Relational.Query

import qualified Models.Player as P

fetchByNumber :: String -> Relation () P.Player
fetchByNumber numStr = relation $ do
  q <- query P.player
  wheres $ q ! P.uniformNumber' .=. value (Just numStr)
  return $ make q

fetchByName :: String -> Relation () P.Player
fetchByName name = relation $ do
  q <- query P.player
  wheres $ q ! P.playerName' .=. value name
  return $ make q

make q = P.Player
         |$| q ! P.id'
         |*| q ! P.playerName'
         |*| q ! P.uniformNumber'
         |*| q ! P.tempUniformNumber'
         |*| q ! P.createdAt'
         |*| q ! P.updatedAt'
