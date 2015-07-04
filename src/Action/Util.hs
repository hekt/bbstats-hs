{-# LANGUAGE OverloadedStrings #-}

module Action.Util where

import           Control.Monad.Trans (liftIO)
import           Data.Aeson (Value)
import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)
import           Network.HTTP.Types.Status
import           Web.Scotty
    
notFoundAction :: ActionM ()
notFoundAction = status status404 >> text "Not Found"

json' :: IO Value -> ActionM ()
json' val = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO val

handleSql :: IConnection conn => IO conn -> (conn -> IO a) -> IO a
handleSql conn act = handleSqlError' $ withConnectionIO conn act
