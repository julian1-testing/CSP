
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CSW where

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



import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace ) 




doHTTPGET url = do
    let settings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000 }
    manager <- newManager settings
    request <- parseRequest url
    response <- httpLbs request manager
    -- Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    return response




doHTTPPost url body = do
    let settings = tlsManagerSettings  {
        managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000
    }
    manager <- newManager settings
    -- get initial request
    initialRequest <- parseRequest url
    -- modify for post
    let request = initialRequest {
        method = BC.pack "POST",
        requestBody = RequestBodyBS $ BC.pack body,
        requestHeaders = [
            (hContentType, BC.pack "application/xml")
        ]
    }
    response <- httpLbs request manager
    Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    return response





parseCSWSummaryRecord = atTag "csw:SummaryRecord" >>>
  proc l -> do
    identifier <- atChildName "dc:identifier" >>> getChildText  -< l
    title      <- atChildName "dc:title" >>> getChildText -< l
    returnA -< (identifier, title)



getCSWIdentifiers s = do
    identifiers <- runX (parseXML s  >>> parseCSWSummaryRecord)
    -- print
    -- mapM (putStrLn.format) identifiers
    mapM (putStrLn.show) identifiers

    putStrLn $ (++) "cws identifiers count: " $ (show.length) identifiers
    return identifiers



-- TODO need to think about the transaction boundary
-- DO NOT COMBINE DATABASSE WITH RETRIEVAL

doCSWGetRecords = do
    let url = "https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/csw"
    -- putStrLn query
    response <- doHTTPPost url queryWMSAndIMOS
    let s = BLC.unpack $ responseBody response
    return s
    where
      -- format (identifier,title) = identifier ++ " -> " ++ title

      queryAll = [r|<?xml version="1.0" encoding="UTF-8"?>
        <csw:GetRecords xmlns:csw="http://www.opengis.net/cat/csw/2.0.2" service="CSW" version="2.0.2"
            resultType="results" startPosition="1" maxRecords="1000" outputFormat="application/xml"  >
          <csw:Query typeNames="csw:Record">
            <csw:Constraint version="1.1.0">
              <Filter xmlns="http://www.opengis.net/ogc" xmlns:gml="http://www.opengis.net/gml">
                <PropertyIsLike wildCard="*" singleChar="_" escape="\\">
                  <PropertyName>AnyText</PropertyName>
                  <Literal>*</Literal>
                </PropertyIsLike>
              </Filter>
            </csw:Constraint>
          </csw:Query>
        </csw:GetRecords>
      |]

      queryWMSAndIMOS = [r|<?xml version="1.0" encoding="UTF-8"?>
        <csw:GetRecords xmlns:csw="http://www.opengis.net/cat/csw/2.0.2" service="CSW" version="2.0.2"
            resultType="results" startPosition="1" maxRecords="1000" outputFormat="application/xml"  >
          <csw:Query typeNames="csw:Record">
            <csw:Constraint version="1.1.0">
              <Filter xmlns="http://www.opengis.net/ogc" xmlns:gml="http://www.opengis.net/gml">
                <And>
                  <PropertyIsLike wildCard="*" singleChar="_" escape="\\">
                    <PropertyName>OnlineResourceType</PropertyName>
                    <Literal>*WMS*</Literal>
                  </PropertyIsLike>
                  <PropertyIsLike wildCard="*" singleChar="_" escape="\\">
                    <PropertyName>PointOfTruth</PropertyName>
                    <Literal>*catalogue-imos.aodn.org.au*</Literal>
                  </PropertyIsLike>
                </And>
              </Filter>
            </csw:Constraint>
          </csw:Query>
        </csw:GetRecords>
      |]


getCSWGetRecordById uuid title = do
    -- TODO - pass the catalog as a parameter - or pre-apply the whole thing.
    putStrLn $ concatMap id [ title, " ", uuid, " ", url ]
    response <- doHTTPGET url
    putStrLn $ "  The status code was: " ++ (show $ statusCode $ responseStatus response)
    let s = BLC.unpack $ responseBody response
    -- putStrLn s
    return s
    where
      url = stripSpace $ [r|
        https://catalogue-portal.aodn.org.au
        /geonetwork/srv/eng/csw
        ?request=GetRecordById
        &service=CSW
        &version=2.0.2
        &elementSetName=full
        &outputSchema=http://www.isotc211.org/2005/gmd
        &id= |] ++ uuid



