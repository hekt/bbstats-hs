{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import           Control.Monad (void)
import           System.Environment (getArgs)
import           Text.Read (readMaybe)

import           Action.Server (server)
import           Action.Reflesh (reflesh)
import           Action.Add (addScore)

main :: IO ()
main = do
  (commandName:args) <- getArgs
  case commandName of
    "server"    -> server
    "reflesh"   -> reflesh
    "add-score" -> addScore (head args)
    _           -> help

help :: IO ()
help = putStrLn "help"
