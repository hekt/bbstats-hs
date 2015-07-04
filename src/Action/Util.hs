module Action.Util where

import           Control.Monad.Trans (liftIO)
import           Data.Aeson (Value)
import           Network.HTTP.Types.Status
import           Web.Scotty
    
json' :: IO Value -> ActionM ()
json' val = do
  setHeader "content-type" "application/json; charset=UTF-8"
  json =<< liftIO val
         
notFoundAction :: ActionM ()
notFoundAction = status status404 >> text "Not Found"
