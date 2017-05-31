{-
    interface for interacting with CSW web-services

    TODO
    - maybe move the CSW parsing to a separate file
    - think about the transaction boundaries when deleting old and storing to db

    refs,
      chef catalogue-imos, POT and WMS
      https://github.com/aodn/chef-private/blob/master/data_bags/imos_webapps_geonetwork_harvesters/catalogue_imos.json

      use csw getrecordbyid to get argo in native schema
            https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=http://www.isotc211.org/2005/gmd


-}

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}

module CSW where

import Text.XML.HXT.Core

import Network.HTTP.Client(responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Text.RawString.QQ


-- TODO tighten?
import Helpers as Helpers



parseCSWSummaryRecord = atTag "csw:SummaryRecord" >>>
  proc l -> do
    identifier <- atChildName "dc:identifier" >>> getChildText  -< l
    title      <- atChildName "dc:title" >>> getChildText -< l
    returnA -< (identifier, title)



doGetRecords url' = do
    {-
        - should parametize OnlineResourceType and PointOfTruth. But this will do for now...
        - Two filters - OnlineResourceType=WMS, PointOfTruth=catalogue-imos.aodn.org.au
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
    -- TODO maybe use post rather than get?
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



testRetrievingAllRecords = do

    -- get records
    result <- doGetRecords "https://catalogue-imos.aodn.org.au/geonetwork"
    let elts = Helpers.parseXML result
    identifiers <- runX (elts >>> parseCSWSummaryRecord)
    mapM (putStrLn.show) identifiers

    -- process each record
    mapM (uncurry doGetRecordById) identifiers

    return ()


main :: IO ()
main = testRetrievingAllRecords




