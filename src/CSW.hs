
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CSW where

import Text.XML.HXT.Core


import Network.HTTP.Client(responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Text.RawString.QQ


-- import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace, doHTTPPost, doHTTPGet ) 
-- import Helpers as Helpers(parseXML, atTag, atChildName, getChildText, stripSpace, doHTTPPost, doHTTPGet ) 
import Helpers as Helpers


-- might be better to move the parsing to a separate file....

-- should this be a tuple ...

parseCSWSummaryRecord = atTag "csw:SummaryRecord" >>>
  proc l -> do
    identifier <- atChildName "dc:identifier" >>> getChildText  -< l
    title      <- atChildName "dc:title" >>> getChildText -< l
    returnA -< (identifier, title)





{--- OK,,, we want to split out the parsing from retrieval...

doGetIdentifiers s = do
    identifiers <- runX (parseXML s  >>> parseCSWSummaryRecord)
    -- print
    -- mapM (putStrLn.format) identifiers
    mapM (putStrLn.show) identifiers

    putStrLn $ (++) "cws identifiers count: " $ (show.length) identifiers
    return identifiers
-}


-- TODO need to think about the transaction boundary
-- DO NOT COMBINE DATABASSE WITH RETRIEVAL


-- "https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/csw"


doGetRecords url' = do
    {-
        - we should make Type and POT arguments but this will do for now...
        - Two filters - OnlineResourceType=WMS, PointOfTruth=catalogue-imos.aodn.org.au
        Should we specify the 
    -}
    let url = url' ++ "/srv/eng/csw"
    putStrLn $ "doGetRcords " ++ url
    response <- doHTTPPost url queryWMSAndIMOS
    let s = BLC.unpack $ responseBody response
    return s
    where
      -- Two filters - OnlineResourceType=WMS, PointOfTruth=catalogue-imos.aodn.org.au
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


doGetRecordById uuid title = do
    -- keeping the title around is pretty useful for output formatting,
    -- TODO - pass the catalog as a parameter - or pre-apply the whole thing.
    putStrLn $ concatMap id [ title, " ", uuid, " ", url ]
    response <- Helpers.doHTTPGet url
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



testGetRecords = do
    result <- doGetRecords "https://catalogue-imos.aodn.org.au/geonetwork" 

    let elts = Helpers.parseXML result

    identifiers <- runX (elts >>> parseCSWSummaryRecord)

    -- parseCSWSummaryRecord = atTag "csw:SummaryRecord" >>>

    -- mapM (putStrLn.show) identifiers
    mapM print identifiers

    -- mapM (\(uuid, title) -> doGetRecordById uuid title) identifiers
    -- doGetRecordById uuid title = do

    -- mapM doGetRecordById identifiers


    return ()


main :: IO ()
main = testGetRecords
{-
  -- testArgoR
  processAllRs conn 
  return ()
-}





