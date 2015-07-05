module Handler.GameScore
    ( get
    , getAll
    , getByDateAndNumber
    , put
    , copyFromCSV
    , getGameIdByDateAndNumber
    ) where

import           Data.Time.Calendar (Day)
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
