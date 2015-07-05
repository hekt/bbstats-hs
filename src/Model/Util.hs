module Model.Util where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.HashMap.Strict ((!))
import qualified Data.Text as T (Text, pack, unpack)
import           Data.Time.Calendar (Day, showGregorian)
import           Data.Time.Format
import qualified Data.Vector as V
import           GHC.Int (Int16)

instance ToJSON Day where
  toJSON = String . T.pack . showGregorian

instance FromJSON Day where
  parseJSON = withText "Day" $ \t ->
    case parseTimeM True defaultTimeLocale "%F" (T.unpack t) of
      Just d -> pure d
      _      -> fail "could not parse ISO-8601 date"

asString :: Object -> T.Text -> Parser String
asString obj key = withArray "array" f $ obj ! key
  where f = pure . show . map conv . V.toList
        conv (Number n) = floor n

asMaybeString :: Object -> T.Text -> Parser (Maybe String)
asMaybeString obj key = let v = obj ! key in case v of
  Null -> pure Nothing
  _    -> withArray "array" f v
    where f = pure . Just . show . map conv . V.toList
          conv (Number n) = floor n

enumParser :: (String -> Int16) -> Object -> T.Text -> Parser Int16
enumParser conv obj key = withText "enum" f $ obj ! key
  where f = pure . conv . T.unpack
