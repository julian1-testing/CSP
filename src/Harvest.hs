{-
  Use CSW and RecordStore modules - to harvest records from CSW server to local db

-}

{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}

module Harvest where

import Text.XML.HXT.Core(runX, (>>>))
import Database.PostgreSQL.Simple as PG(connectPostgreSQL, close)
import Text.RawString.QQ

import qualified Helpers as Helpers
import qualified CSW as CSW
-- import qualified Record as R(Record, Record(..),DataIdentification(..),showRecord )
-- import qualified Record as R(Record, dataIdentification, transferLinks, showRecord, Record)
import qualified Record --as Record(Record(..))--(Record, dataIdentification, transferLinks, showRecord, Record)
import qualified RecordStore as RS
import qualified ParseMCP20 as ParseMCP20


-- should have a simple function to do something if it's the right

applyEither lf rf x 
  = case x of
    Right a -> lf a
    Left a -> rf a


----------------

doGetAndProcessRecord conn uuid title = do

    print $ "doGetAndProcessRecord " ++ uuid ++ " " ++ title


    recordText <- CSW.doGetRecordById uuid title

    let elts = Helpers.parseXML recordText
    myRecord <- ParseMCP20.parse elts 

    -- TODO if showRecord was just show, then we wouldn't have to destructure
    {-
    putStrLn $ case myRecord of
        Right record -> 
          R.showRecord record
        Left msg -> 
          msg
    -}

  -- change name store to store
  -- uuid should match....
  -- 

    -- myRecord <- ParseMCP20.parse elts
    case myRecord of
        Right record -> do

            putStrLn $ Record.showRecord record --myRecord

            -- storeRecordUUID conn uuid title = do
            conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
            record_id <- RS.storeRecordUUID conn "myuuid" 
            RS.storeDataIdentification conn record_id (Record.dataIdentification record)
            RS.storeTransferLinks conn record_id (Record.transferLinks record) 

            -- PG.close conn

            return ()


{-
    record_id <- RS.storeRecordUUID conn uuid
    RS.storeDataIdentification conn record_id (Record.dataIdentification record)
    RS.storeTransferLinks conn record_id (Record.transferLinks record) 
-}



    putStrLn $ applyEither Record.showRecord id myRecord 

    
    -- OK, want to store the damn thing to the db...

    return ()




doGetAndProcessRecords conn = do
    {-
        -- THIS IS NOT RIGHT - NEEDS TO be a deletion as we do each record 
        -- and need to know if a record has been removed.
        PG.execute conn "delete from resource *" ()
        PG.execute conn "delete from facet *" ()
        PG.execute conn "delete from record *" ()
    -}
    
    -- get records to process
    result <- CSW.doGetRecords "https://catalogue-imos.aodn.org.au/geonetwork" 
    let elts = Helpers.parseXML result
    identifiers <- runX (elts >>> CSW.parseCSWSummaryRecord)
    mapM (putStrLn.show) identifiers
 
    -- process each record
    mapM (uncurry $ doGetAndProcessRecord conn) identifiers



main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  doGetAndProcessRecords conn 
  PG.close conn

  return ()



