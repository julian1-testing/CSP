-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MetadataRecord where


import Text.XML.HXT.Core
import Database.PostgreSQL.Simple as PG(query, execute, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import Text.RawString.QQ

import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace)

-- IMPORTANT must close!!!
-- responseClose :: Response a -> IO ()


{-
    we work out what we need by looking at
        1. MetaDataRecord.js and seeing the fields
        2. looking at the fields in the response.xml
        3. then mapping the values in response.xml back into original argo.xml record to get tag name
-}



{-
    ./web-app/js/portal/data/MetadataRecord.js
        title               done
        abstract            done
        uuid                 done
        parameterField   -   done db join vocab  parameters we have - just join in the db and format
        platform         -   done db field join vocab

        organisationField    **** don't have yet - .

        jurisdictionLink    done
        licenseLink         done
        licenseName         done
        licenseImagelink    done

        attrConstrField -> attrConstr ->
            - more than one in MD_Commons

        otherConstrField ->  also in MDcommons

        otherCitation      ***** does not appear in summary-response
        useLimitationiField done -> useLimitation - more than one

        temperalExtentBegin done -> tempExtentBegin  -> it's just a date  eg. <tempExtentBegin>1999-10-01t00:00:00.000z</tempExtentBegin>

        linksField          done -> links -> CI_Online_Resource  eg. 'View and download'  transferLinks 
        linkedFilesField  -> linkedFiles -> ****** does not appear 
        onlineResourcesFiled -> onlineResources ***** does not appear 
        pointOfTruthLinkField -> pointOfTruthLink  **** does not appear 
        bboxField -> bbox -> 
                            -> geoBox  ** don't have for argo.... but do for satellite... i think
                            -> geoPolygon  done.

        wmsLayer             **** does not appear 
        iconSourceUuid   -> source -> which is the uuid 
-}



data Identification = Identification { 

    title:: String, 
    abstract:: String, 
    jurisdictionLink :: String, 
    licenseLink :: String , 
    licenseName :: String , 
    licenseImageLink:: String
    } deriving (Show, Eq)


data TransferLink = TransferLink {

    protocol :: String,
    linkage :: String,
    description :: String
} deriving (Show, Eq)




data DataParameter = DataParameter {

    txt :: String,
    url :: String
} deriving (Show, Eq)






{-

data MCP2Record = MCP2Record {

    -- data parameters
    dataParameters <- runX (parsed >>> parseDataParameters)
    mapM print dataParameters

    -- identifier
    identifier <- runX (parsed >>> parseFileIdentifier)
    print identifier


    -- dataIdentification
    putStrLn "\n###### data identification stuff"
    identification <- runX (parsed >>> parseDataIdentification )
    print identification

    -- dataIdentification
    putStrLn "\n###### attribution constraints "
    attrConstraints <- runX (parsed >>> parseAttributionConstraints)
    mapM print attrConstraints

    putStrLn "\n###### useLimitations"
    useLimitations <- runX (parsed >>> parseUseLimitations)
    mapM print useLimitations


    putStrLn "\n###### temporal begin"
    temporalBegin <- runX (parsed >>> parseTemporalExtentBegin )
    mapM print temporalBegin


    putStrLn "\n###### links"
    links <- runX (parsed >>> parseTransferLinks)
    mapM print links

    
    putStrLn "\n###### geoPoly "
    geoPoly <- runX (parsed >>> parseGeoPolygon )
    mapM print geoPoly
-} 

parseFileIdentifier =
  atTag "gmd:fileIdentifier" >>>
  proc identifier -> do
    uuid <- atChildName "gco:CharacterString" >>> getChildText -< identifier
    returnA -< (uuid)



parseDataIdentification =
  -- single instance
  atTag "mcp:MD_DataIdentification" >>>
  proc dataIdent -> do

    title <- atChildName "gmd:citation" >>> atChildName "gmd:CI_Citation" >>> atChildName "gmd:title"
        >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent

    abstract <- atChildName "gmd:abstract" >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent

    -- and only one md_commons
    md_commons <- atChildName "gmd:resourceConstraints" >>> atChildName "mcp:MD_Commons" -< dataIdent

    jurisdictionLink <- atChildName "mcp:jurisdictionLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons

    licenseLink <- atChildName "mcp:licenseLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons

    licenseName <- atChildName "mcp:licenseName" >>> atChildName "gco:CharacterString" >>> getChildText -< md_commons

    licenseImageLink <- atChildName "mcp:imageLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons

    -- change name to DataIdentification
    returnA -< Identification {
        title = title,
        abstract = abstract,
        jurisdictionLink = jurisdictionLink,
        licenseLink = licenseLink,
        licenseName = licenseName,
        licenseImageLink = licenseImageLink
        }



parseAttributionConstraints =
  -- have more than one
  atTag "gmd:resourceConstraints"  >>> atChildName "mcp:MD_Commons" >>>
  proc md_commons -> do
    attrConstr <- atChildName "mcp:attributionConstraints" >>> atChildName "gco:CharacterString" >>> getChildText -< md_commons
    returnA -< attrConstr



parseUseLimitations =
  -- have more than one
  atTag "gmd:resourceConstraints"  >>> atChildName "gmd:MD_Constraints" >>>
  proc md_commons -> do
    useLimitation <- atChildName "gmd:useLimitation" >>> atChildName "gco:CharacterString" 
        >>> getChildText -< md_commons
    returnA -< useLimitation



parseTemporalExtentBegin =
  -- once
  atTag "gmd:extent"  >>> atChildName "gmd:EX_Extent" >>>
  proc extent -> do
    begin <- atChildName "gmd:temporalElement" >>> atChildName "mcp:EX_TemporalExtent"
        >>> atChildName "gmd:extent" >>> atChildName "gml:TimePeriod"
        >>> atChildName "gml:begin" >>> atChildName "gml:TimeInstant"
        >>> atChildName "gml:timePosition" >>> getChildText -< extent
    returnA -< begin



parseGeoPolygon =
    -- change name  
  -- multiple
  atTag "gmd:extent"  >>> atChildName "gmd:EX_Extent" >>>
  proc extent -> do
    begin <- atChildName "gmd:geographicElement" >>> atChildName "gmd:EX_BoundingPolygon"
        >>> atChildName "gmd:polygon" >>> atChildName "gml:Polygon"
        >>> atChildName "gml:exterior" >>> atChildName "gml:LinearRing"
        >>> atChildName "gml:posList" >>> getChildText -< extent
    returnA -< begin



-- change name to links...
parseTransferLinks =
  atTag "gmd:transferOptions" >>> atChildName "gmd:MD_DigitalTransferOptions" >>>
  proc transfer -> do

    resource <- atChildName "gmd:onLine" >>> atChildName "gmd:CI_OnlineResource" -<  transfer

    protocol    <- atChildName "gmd:protocol" >>> atChildName "gco:CharacterString" >>> getChildText  -< resource
    linkage     <- atChildName "gmd:linkage"  >>> atChildName "gmd:URL" >>> getChildText -< resource
    description <- atChildName "gmd:description" >>> atChildName "gco:CharacterString" >>> getChildText -< resource
    returnA -< TransferLink { 
        protocol = protocol, 
        linkage = linkage, 
        description = description 
    }




parseDataParameters =
  atTag "mcp:DP_Term" >>>
  proc term -> do
    txt  <- atChildName "mcp:term"  >>> atChildName "gco:CharacterString" >>> getChildText -< term
    url  <- atChildName "mcp:vocabularyTermURL"  >>> atChildName "gmd:URL" >>> getChildText -< term
    returnA -< (txt, url)






----------------

processRecordUUID conn uuid title = do
  PG.execute conn "insert into record(uuid,title) values (?, ?)"
    (uuid :: String, title :: String)






----------------
-- resources
{-

processOnlineResource conn uuid (protocol,linkage, description) = do
    PG.execute conn [r|
      insert into resource(record_id,protocol,linkage, description)
      values (
        (select id from record where uuid = ?), ?, ?, ?
      )
    |] (uuid :: String, protocol :: String, linkage, description)



processOnlineResources conn uuid recordText = do
    onlineResources <- runX (parseXML recordText >>> parseTransferLinks)
    putStrLn $ (++) "resource count: " $ (show.length) onlineResources
    mapM (putStrLn.show) onlineResources
    mapM (processOnlineResource conn uuid) onlineResources
-}

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
        PG.execute conn [r|
          insert into facet(concept_id, record_id)
          values (?, (select record.id from record where record.uuid = ?))
          on conflict
          do nothing
        |] (concept_id :: Integer, uuid :: String)
        return ()

      0 -> putStrLn $ "dataParameter '" ++ url ++ "' not found!"
      _ -> putStrLn $ "dataParameter '" ++ url ++ "' found multiple matches?"


processDataParameters conn uuid recordText = do

    -- TODO IMPORTANT - should remove the uuid first...
    dataParameters <- runX (parseXML recordText >>> parseDataParameters)
    putStrLn $ "data parameter count: " ++ (show.length) dataParameters
    mapM (putStrLn.show) dataParameters
    mapM (processDataParameter conn uuid) dataParameters


-- ok we want to push this stuff into a data structure...

testArgoRecord = do
    recordText <- readFile "./test-data/argo.xml"

    let parsed = Helpers.parseXML recordText

    -- data parameters
    dataParameters <- runX (parsed >>> parseDataParameters)
    mapM print dataParameters

    -- identifier
    identifier <- runX (parsed >>> parseFileIdentifier)
    print identifier


    -- dataIdentification
    putStrLn "\n###### data identification stuff"
    identification <- runX (parsed >>> parseDataIdentification )
    print identification

    -- dataIdentification
    putStrLn "\n###### attribution constraints "
    attrConstraints <- runX (parsed >>> parseAttributionConstraints)
    mapM print attrConstraints

    putStrLn "\n###### useLimitations"
    useLimitations <- runX (parsed >>> parseUseLimitations)
    mapM print useLimitations


    putStrLn "\n###### temporal begin"
    temporalBegin <- runX (parsed >>> parseTemporalExtentBegin )
    mapM print temporalBegin


    putStrLn "\n###### links"
    links <- runX (parsed >>> parseTransferLinks)
    mapM print links

    
    putStrLn "\n###### geoPoly "
    geoPoly <- runX (parsed >>> parseGeoPolygon )
    mapM print geoPoly
 

    return ()


main = testArgoRecord

