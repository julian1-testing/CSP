{-
    Use CSW and RecordStore modules - to harvest records from CSW catalog service to a local db

-}

{-# LANGUAGE OverloadedStrings #-}

module Harvest where

import qualified Database.PostgreSQL.Simple as PG(connectPostgreSQL, close)
import qualified Data.ByteString.Char8 as BS(pack)
import Text.XML.HXT.Core(runX, (>>>))
import Text.RawString.QQ


import qualified Helpers as Helpers
import qualified CSW as CSW
import qualified Record as R
import qualified RecordStore as RS
import qualified ParseMCP20 as ParseMCP20

import qualified Config as R
----------------

doGetAndProcessRecord conn source' uuid title = do

    print $ "doGetAndProcessRecord " ++ uuid ++ " " ++ title

    recordText <- CSW.doGetRecordById uuid title
    record <- ParseMCP20.parse $ Helpers.parseXML recordText

    -- set the source
    -- TODO - tidy this Maybe BS handling
    let record' = record { R.source = source' >>= (return.BS.pack) }  

    RS.storeAll conn record'
    return ()



doGetAndProcessRecords conn = do

    -- delete current index for all records
    RS.deleteAll conn

    let source = "https://sourceue-imos.aodn.org.au/geonetwork"

    -- get records to process
    result <- CSW.doGetRecords source 
    let elts = Helpers.parseXML result
    identifiers <- runX (elts >>> CSW.parseCSWSummaryRecord)

    mapM (putStrLn.show) identifiers

    -- process each record
    mapM (uncurry $ doGetAndProcessRecord conn $ Just source) identifiers



main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  doGetAndProcessRecords conn
  PG.close conn

