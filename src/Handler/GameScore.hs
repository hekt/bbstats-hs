module Handler.GameScore
    ( get
    , getAll
    , getByDateAndNumber
    , put
    , copyFromCSV
    , getGameIdByDateAndNumber
    , putFromCSVAndGetId
    ) where

import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BS
import           Data.Csv (decodeByName)
import           Data.Time.Calendar (Day)
import qualified Data.Vector as V
import           Database.HDBC.Record (runInsertQuery)
import           Database.HDBC.Types (IConnection, runRaw)
import           Database.Relational.Query hiding (id')
import           GHC.Int (Int16, Int32)
import           System.Directory (makeAbsolute)

import           Handler.Util
import           Model.GameScore
import           Query.GameScore

get :: IConnection conn => conn -> Int32 -> IO (Maybe GameScore)
get conn = fetch conn .  find
    
getAll :: IConnection conn => conn -> IO [GameScore]
getAll conn = fetchAll' conn findAll

getByDateAndNumber :: IConnection conn
                   => conn -> Day -> Int16 -> IO (Maybe GameScore)
getByDateAndNumber conn d = fetch conn . findByDateAndNumber d

getGameIdByDateAndNumber :: IConnection conn
                         => conn -> Day -> Int16 -> IO (Maybe Int32)
getGameIdByDateAndNumber conn d n = fetch conn q
  where q = relation $ do
          g <- query $ findByDateAndNumber d n
          return $ g ! id'

put :: IConnection conn => conn -> GameScoreP -> IO Integer
put conn g = runInsertQuery conn (persist g) ()

copyFromCSV :: IConnection conn => conn -> FilePath -> IO ()
copyFromCSV conn path = do
  absPath <- makeAbsolute path
  runRaw conn $ copySql tableName insertColumnNames absPath

{-
 - CSV から gameDate と gameNumber を取得しなければならないため
 - Util の関数を使わない
 -}
putFromCSVAndGetId :: IConnection conn => conn
                   -> FilePath -> IO (Either String Int32)
putFromCSVAndGetId conn path = runExceptT $ do
  csvBs    <- liftIO $ BS.readFile path
  g        <- ExceptT . return $ V.head . snd <$> decodeByName csvBs
  let date = pGameDate g
      num  = pGameNumber g
  liftIO $ put conn g
  maybeId  <- liftIO $ getGameIdByDateAndNumber conn date num
  ExceptT . return $ maybe (Left "failed to get inserted record") Right maybeId
