-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}


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


import Database.PostgreSQL.Simple

import Text.RawString.QQ

import Data.Char (isSpace)

{-
  catalogue-imos, pot, and WMS
  https://github.com/aodn/chef-private/blob/master/data_bags/imos_webapps_geonetwork_harvesters/catalogue_imos.json

  csw getrecordbyid request,
  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=http://www.isotc211.org/2005/gmd

-- ok now we want to go through the actual damn records,



-}

-- import qualified Prelude as P

-- IMPORTANT must close!!!
-- responseClose :: Response a -> IO ()


parseXML s = readString [ withValidate no
    , withRemoveWS yes  -- throw away formating WS
    ] s


atTag tag = deep (isElem >>> hasName tag)

atChildName s = getChildren >>> hasName s

getChildText = getChildren >>> getText

stripSpace = filter $ not.isSpace



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


-- ok, we god a 120 records which is pretty nice.




parseOnlineResources = 
  atTag "gmd:CI_OnlineResource" >>>
  proc l -> do
    protocol    <- atChildName "gmd:protocol" >>> atChildName "gco:CharacterString" >>> getChildText  -< l
    linkage     <- atChildName "gmd:linkage"  >>> atChildName "gmd:URL" >>> getChildText -< l
    description <- atChildName "gmd:description" >>> atChildName "gco:CharacterString" >>> getChildText -< l
    returnA -< (protocol, linkage, description)


parseDataParameters = 
  atTag "mcp:dataParameter" >>>
  proc l -> do
    term <- atChildName "mcp:DP_DataParameter" >>> atChildName "mcp:parameterName" >>> atChildName "mcp:DP_Term" -< l
    txt  <- atChildName "mcp:term"  >>> atChildName "gco:CharacterString" >>> getChildText -< term
    url  <- atChildName "mcp:vocabularyTermURL"  >>> atChildName "gmd:URL" >>> getChildText -< term
    returnA -< (txt, url)






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



----------------

processRecordUUID conn uuid title = do
  execute conn "insert into record(uuid,title) values (?, ?)"   
    (uuid :: String, title :: String)


----------------
-- resources

processOnlineResource conn uuid (protocol,linkage, description) = do
    execute conn [r|
      insert into resource(record_id,protocol,linkage, description) 
      values (
        (select id from record where uuid = ?), ?, ?, ?
      )
    |] (uuid :: String, protocol :: String, linkage, description)



processOnlineResources conn uuid recordText = do
    onlineResources <- runX (parseXML recordText >>> parseOnlineResources)
    putStrLn $ (++) "resource count: " $ (show.length) onlineResources
    mapM (putStrLn.show) onlineResources
    mapM (processOnlineResource conn uuid) onlineResources


----------------
-- data parameters

processDataParameter conn uuid (term, url) = do
    -- look up the required concept
    xs :: [ (Integer, String) ] <- query conn "select id, label from concept where url = ?" (Only url)
    -- putStrLn $ (show.length) xs 
    case length xs of
      1 -> do
        -- store the concept
        let (concept_id, concept_label) : _ = xs
        execute conn [r|
          insert into facet(concept_id, record_id) 
          values (?, (select record.id from record where record.uuid = ?))
        |] (concept_id :: Integer, uuid :: String) 
        return ()
        
      0 -> putStrLn "dataParameter not found"
      _ -> putStrLn "multiple dataParameters?"


processDataParameters conn uuid recordText = do
    dataParameters <- runX (parseXML recordText >>> parseDataParameters)
    putStrLn $ (++) "data parameter count: " $ (show.length) dataParameters
    mapM (putStrLn.show) dataParameters
    mapM (processDataParameter conn uuid) dataParameters



----------------

processRecord conn (uuid, title) = do
    record <- getCSWGetRecordById uuid title 
    processRecordUUID conn uuid title 
    processDataParameters conn uuid record
    processOnlineResources conn uuid record
    return ()


-- TODO - store the origin catalog .... that we scanned . in fact that 
-- might be stored in the db, and is ther

-- OK. we should probably let it go to completion - just to test...
----------------


main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  -- execute conn "truncate resource;"  ()
  -- note that the sequence will update -
  execute conn "delete from resource *" ()
  execute conn "delete from facet *" ()
  execute conn "delete from record *" ()

  -- TODO - seperate out query and parse action - 
  -- do query and get records
  identifiers <- doCSWGetRecords

  s <- doCSWGetRecords 

  identifiers <- getCSWIdentifiers s 

  mapM (processRecord conn) identifiers

  return ()

