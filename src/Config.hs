

module Config where

import qualified Database.PostgreSQL.Simple as PG( defaultConnectInfo, ConnectInfo(..))


connectionInfo :: PG.ConnectInfo
connectionInfo = PG.defaultConnectInfo { 
      PG.connectHost = "postgres.localnet"
    , PG.connectPort = 5432
    , PG.connectDatabase = "harvest"
    , PG.connectUser = "harvest"
    , PG.connectPassword = "harvest"
}



