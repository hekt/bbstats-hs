{-# LANGUAGE FlexibleContexts #-}

module Query
    ( fetchPlayerByNumber
    ) where

import Database.Relational.Query

import qualified Models.Player as Player

fetchPlayerByNumber :: String -> Relation () Player.Player
fetchPlayerByNumber numStr = relation $ do
  p <- query Player.player
  wheres $ p ! Player.uniformNumber' .=. value (Just numStr)
  return $ player p

player p = Player.Player
         |$| p ! Player.id'
         |*| p ! Player.playerName'
         |*| p ! Player.uniformNumber'
         |*| p ! Player.tempUniformNumber'
         |*| p ! Player.createdAt'
         |*| p ! Player.updatedAt'
