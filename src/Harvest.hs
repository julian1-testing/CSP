{-
  Use CSW and RecordStore modules - to harvest records from CSW server to local db

-}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Harvest where

import Text.XML.HXT.Core(runX, (>>>))

-- import Database.PostgreSQL.Simple as PG(query, execute, connectPostgreSQL)
import Database.PostgreSQL.Simple as PG(connectPostgreSQL, close)
-- import Database.PostgreSQL.Simple.Types as PG(Only(..))

import Text.RawString.QQ

import qualified CSW as CSW
import qualified Record as R
import qualified RecordStore as RS

import qualified Helpers as Helpers
-- import qualified Helpers as Helpers(parseXML) 

----------------

doGetAndprocessRecord conn uuid title = do
    -- TODO IMPORTANT - should remove the uuid first...
    -- TODO - VERY IMPORTANT we should separate out the CSW action of getting the record 
    -- removing the old stuff and indexing resources and parameters,

    print "hi"
{-
    record <- CSW.doGetRecordById uuid title
    RS.processRecordUUID conn uuid title
    RS.processDataParameters conn uuid record
    RS.processOnlineResources conn uuid record
-}
    return ()




doGetAndProcessRecords conn = do
    -- this is not very nice.... - should do deletion incrementallly for each record
    -- and transactionally
{-
    PG.execute conn "delete from resource *" ()
    PG.execute conn "delete from facet *" ()
    PG.execute conn "delete from record *" ()
-}
    

    result <- CSW.doGetRecords "https://catalogue-imos.aodn.org.au/geonetwork" 
    let elts = Helpers.parseXML result
    identifiers <- runX (elts >>> CSW.parseCSWSummaryRecord)
    mapM (putStrLn.show) identifiers
 
    -- TODO what's happening here,
    -- s <- CSW.doGetRecords -- change name Parse records parseIdentidifers
    -- identifiers <- CSW.doGetIdentifiers s
    -- mapM (processRecord conn) identifiers
    -- mapM print identifiers
    -- print identifiers


    mapM (uncurry $ doGetAndprocessRecord conn) identifiers



main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- testArgoR

  doGetAndProcessRecords conn 


  PG.close conn

  return ()



{-
  -- execute conn "truncate resource;"  ()
  -- note that the sequence will update -
  execute conn "delete from resource *" ()
  execute conn "delete from facet *" ()
  execute conn "delete from record *" ()
-}



{-
  -- TODO - seperate out query and parse action -
  -- do query and get records
  identifiers <- doCSWGetRs

  s <- doCSWGetRs

  identifiers <- doGetIdentifiers s

  -- IMPORTANT - we should have a single function...
  mapM (processR conn) identifiers
-}

