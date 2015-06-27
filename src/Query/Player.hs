{-# LANGUAGE FlexibleContexts #-}

module Query.Player
    ( fetchAll
    , fetchByNumber
    , fetchByName
    ) where

import Database.Relational.Query

import qualified Model.Player as M

fetchAll :: Relation () M.Player
fetchAll = relation $ query M.mstPlayer >>= return . make

fetchByNumber :: String -> Relation () M.Player
fetchByNumber numStr = relation $ do
  q <- query M.mstPlayer
  wheres $ q ! M.uniformNumber' .=. value (Just numStr)
  return $ make q

fetchByName :: String -> Relation () M.Player
fetchByName name = relation $ do
  q <- query M.mstPlayer
  wheres $ q ! M.playerName' .=. value name
  return $ make q

make q = M.MstPlayer
         |$| q ! M.id'
         |*| q ! M.playerName'
         |*| q ! M.uniformNumber'
         |*| q ! M.tempUniformNumber'
         |*| q ! M.createdAt'
         |*| q ! M.updatedAt'
