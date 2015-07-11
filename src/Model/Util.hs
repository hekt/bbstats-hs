{-# LANGUAGE OverloadedStrings #-}

module Model.Util where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv as Csv
import           Data.HashMap.Strict ((!))
import qualified Data.Text as T (Text, pack, unpack)
import           Data.Time.Calendar (Day, showGregorian)
import           Data.Time.Format
import qualified Data.Vector as V
import           GHC.Int (Int16)
import           Text.Read (readMaybe)

instance ToJSON Day where
  toJSON = String . T.pack . showGregorian

instance FromJSON Day where
  parseJSON = withText "Day" $ \t ->
    case parseTimeM True defaultTimeLocale "%F" (T.unpack t) of
      Just d -> pure d
      _      -> fail "could not parse ISO-8601 date"

instance Csv.FromField Day where
  parseField m = case parseTimeM True defaultTimeLocale "%F" (BS.unpack m) of
    Just d -> pure d
    _      -> fail "could not parse ISO-8601 date"

instance Csv.FromField Bool where
  parseField = pure . str2bool . BS.unpack

str2bool :: String -> Bool
str2bool = let falsey = ["0", "f", "false", "False", "FALSE"]
           in not . flip elem falsey
