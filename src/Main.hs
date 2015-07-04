{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import           System.Environment (getArgs)

import           Action.Server (server)
import           Action.Reflesh (reflesh)

main :: IO ()
main = do
  (commandName:args) <- getArgs
  case commandName of
    "server"  -> server
    "reflesh" -> reflesh
    _         -> help

help :: IO ()
help = putStrLn "help"
