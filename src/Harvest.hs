
-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Harvest where

import Text.XML.HXT.Core

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

-- TODO import qualified
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

import Data.Char(isSpace)

import Database.PostgreSQL.Simple as PG(query, execute, connectPostgreSQL)

-- http://stackoverflow.com/questions/34547937/haskell-import-qualified-and-not-in-scope-data-constructor
import Database.PostgreSQL.Simple.Types as PG(Only(..))


import Text.RawString.QQ



import qualified CSW
import qualified Record

-- import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace ) 
import Helpers(parseXML) 



----------------

processRecord conn (uuid, title) = do
    -- TODO IMPORTANT - should remove the uuid first...
    -- TODO - VERY IMPORTANT we should separate out the CSW action of getting the record 
    -- removing the old stuff and indexing resources and parameters,
    record <- CSW.getCSWGetRecordById uuid title
    Record.processRecordUUID conn uuid title
    Record.processDataParameters conn uuid record
    Record.processOnlineResources conn uuid record
    return ()





processAllRecords conn = do
    -- this is not very nice.... - should do deletion incrementallly for each record
    -- and transactionally
    PG.execute conn "delete from resource *" ()
    PG.execute conn "delete from facet *" ()
    PG.execute conn "delete from record *" ()
    
    identifiers <- CSW.doCSWGetRecords
    -- TODO what's happening here,
    s <- CSW.doCSWGetRecords
    identifiers <- CSW.getCSWIdentifiers s
    mapM (processRecord conn) identifiers


testArgoRecord = do
    recordText <- readFile "./examples/argo.xml" 
    dataParameters <- runX (Helpers.parseXML recordText >>> Record.parseDataParameters)
    mapM print dataParameters
 




main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- testArgoRecord

  processAllRecords conn 
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
  identifiers <- doCSWGetRecords

  s <- doCSWGetRecords

  identifiers <- getCSWIdentifiers s

  -- IMPORTANT - we should have a single function...
  mapM (processRecord conn) identifiers
-}

