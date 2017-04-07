
-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Harvest where

import Text.XML.HXT.Core(runX, (>>>))

import Database.PostgreSQL.Simple as PG(query, execute, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))

import Text.RawString.QQ

import qualified CSW as CSW
import qualified MetadataRecord as MD
-- import qualified Helpers as Helpers(parseXML) 

{-
  catalogue-imos, pot, and WMS
  https://github.com/aodn/chef-private/blob/master/data_bags/imos_webapps_geonetwork_harvesters/catalogue_imos.json

  csw getrecordbyid request,
  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetMDById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=http://www.isotc211.org/2005/gmd


-}


----------------

processMD conn (uuid, title) = do
    -- TODO IMPORTANT - should remove the uuid first...
    -- TODO - VERY IMPORTANT we should separate out the CSW action of getting the record 
    -- removing the old stuff and indexing resources and parameters,
    record <- CSW.getRecordById uuid title
    MD.processRecordUUID conn uuid title
    MD.processDataParameters conn uuid record
    MD.processOnlineResources conn uuid record
    return ()





processAllMDs conn = do
    -- this is not very nice.... - should do deletion incrementallly for each record
    -- and transactionally
    PG.execute conn "delete from resource *" ()
    PG.execute conn "delete from facet *" ()
    PG.execute conn "delete from record *" ()
    
    identifiers <- CSW.doGetRecords
    -- TODO what's happening here,
    s <- CSW.doGetRecords
    identifiers <- CSW.getIdentifiers s
    mapM (processMD conn) identifiers






main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- testArgoMD

  processAllMDs conn 
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
  identifiers <- doCSWGetMDs

  s <- doCSWGetMDs

  identifiers <- getIdentifiers s

  -- IMPORTANT - we should have a single function...
  mapM (processMD conn) identifiers
-}

