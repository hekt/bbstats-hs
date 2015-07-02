module Service.Util where

import           Database.HDBC.Session (withConnectionIO, handleSqlError')
import           Database.HDBC.Types (IConnection)

handle :: IConnection conn => IO conn -> (conn -> IO a) -> IO a
handle conn act = handleSqlError' $ withConnectionIO conn act
