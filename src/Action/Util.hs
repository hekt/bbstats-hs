{-# LANGUAGE OverloadedStrings #-}

module Action.Util where

import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Maybe
import           Data.Aeson (Value)
import           Data.Time.Calendar (Day)
import           Database.HDBC.Types (IConnection)
import           GHC.Int (Int16)
import           Network.HTTP.Types.Status
import           System.Directory (canonicalizePath)
import           System.FilePath.Posix (takeBaseName)
import           Text.Regex.Posix ((=~~))
import           Web.Scotty
    
notFoundAction :: ActionM ()
notFoundAction = status status404 >> text "Not Found"

json' :: IO Value -> ActionM ()
json' val = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO val

getDateAndNumberFromFilePath :: FilePath -> IO (Maybe (Day, Int16))
getDateAndNumberFromFilePath path = do
  dirName <- takeBaseName <$> canonicalizePath path
  let reStr = "([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9]+)" :: String
  case dirName =~~ reStr of
    Just [[_, day, num]] -> return $ Just (read day, read num)
    _                    -> return Nothing
