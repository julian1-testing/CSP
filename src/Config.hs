
-- TODO change name to ConfigTest? 

{-# LANGUAGE OverloadedStrings #-}


module Config where

import qualified Data.ByteString.Char8 as BS(ByteString(..))

import qualified Database.PostgreSQL.Simple as PG( defaultConnectInfo, ConnectInfo(..))


-- TODO remove this...
connString :: BS.ByteString
connString = "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"



connectionInfo :: PG.ConnectInfo
connectionInfo = PG.defaultConnectInfo { 
      PG.connectHost = "postgres.localnet"
    , PG.connectPort = 5432
    , PG.connectDatabase = "harvest"
    , PG.connectUser = "harvest"
    , PG.connectPassword = "harvest"
}



