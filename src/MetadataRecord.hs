-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
-- {-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}

module MetadataRecord where


import Text.XML.HXT.Core
{-
import Database.PostgreSQL.Simple as PG(query, execute, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import Text.RawString.QQ
-}

import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace)

-- IMPORTANT must close resources !!!
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


-- If any field is genuinely optional then should use Maybe 

data DataIdentification = DataIdentification { 

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

    term :: String,
    url :: String
} deriving (Show, Eq)


-- change name - PortalRecord, or MCP2 Record
data Record = Record {

    uuid :: String,
    dataIdentification :: DataIdentification , 
    attrConstraints :: [ String ],
    useLimitations :: [ String ],
    dataParameters :: [ DataParameter ], 
    temporalBegin :: String,
    links :: [ TransferLink ],
    geoPoly :: [ String ]
} deriving (Show, Eq)


-- ByteString?

-- should pass in the formatting function to use....



showRecord myRecord =

    let formatList f xs = concatMap id $ map (\x ->  "\n  -" ++ f x) xs in

    concatMap id [ 
        "uuid= " ++ uuid  myRecord, "\n",

        -- TODO -- tidy        
        "dataIdentification.title= " ++ (show $ (title.dataIdentification) myRecord), "\n",
        "dataIdentification.abstract= " ++ (show $ (abstract.dataIdentification) myRecord), "\n",
        "dataIdentification.jurisdictionLinke = " ++ (show $ (jurisdictionLink.dataIdentification) myRecord), "\n",
        "dataIdentification.licenseLink= " ++ (show $ (licenseLink.dataIdentification) myRecord), "\n",
        "dataIdentification.licenseName= " ++ (show $ (licenseName.dataIdentification) myRecord), "\n",
        "dataIdentification.licenseImageLink = " ++ (show $ (licenseImageLink.dataIdentification) myRecord), "\n",
 
        "attrConstraints= ", formatList id (attrConstraints myRecord), "\n",
        "useLimitations= ", formatList id (useLimitations myRecord), "\n",
        "dataParameters= ", formatList show (dataParameters myRecord), "\n",
        "temporalBegin= ",  temporalBegin myRecord, "\n",
        "links= ", formatList show (links myRecord), "\n",
        "geoPoly= ", formatList id (geoPoly myRecord), "\n"
    ]



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

    -- maybe change name to DataIdentification
    returnA -< DataIdentification {
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
    term_ <- atChildName "mcp:term"  >>> atChildName "gco:CharacterString" >>> getChildText -< term
    url  <- atChildName "mcp:vocabularyTermURL"  >>> atChildName "gmd:URL" >>> getChildText -< term
    returnA -< DataParameter { 
        term = term_,
        url = url 
    }




----------------






----------------
-- TODO move all this
{-

processRecordUUID conn uuid title = do
  PG.execute conn "insert into record(uuid,title) values (?, ?)"
    (uuid :: String, title :: String)



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
-}

-- ok we want to push this stuff into a data structure...


parseMCP20 elts = do

    identifier <- runX (elts >>> parseFileIdentifier)

    dataIdentification <- runX (elts >>> parseDataIdentification )

    attrConstraints <- runX (elts >>> parseAttributionConstraints)

    useLimitations <- runX (elts >>> parseUseLimitations)

    dataParameters <- runX (elts >>> parseDataParameters)

    temporalBegin <- runX (elts >>> parseTemporalExtentBegin )

    links <- runX (elts >>> parseTransferLinks)

    geoPoly <- runX (elts >>> parseGeoPolygon )

    -- avoid throwing, or perhaps - if genuinely optional then use Maybe 
    let headWithDefault x d = case not $ null x of 
            True -> head x
            False -> d

    -- TODO check we have a DataIdentification section

    let myRecord = Record {
        -- if the list is empty??????   need to test.... 
        uuid = headWithDefault identifier "blah",
        dataIdentification = head dataIdentification,     -- t dangerous... fixme  uuid should never be null, since searchable index
        attrConstraints = attrConstraints ,
        useLimitations = useLimitations,
        dataParameters = dataParameters,
        temporalBegin = headWithDefault temporalBegin "unknown",
        links = links,
        geoPoly = geoPoly
    }

    return myRecord 


testArgoRecord = do

    recordText <- readFile "./test-data/argo.xml"
    let elts = Helpers.parseXML recordText

    myRecord <- parseMCP20 elts 
    putStrLn $ showRecord myRecord
    return ()

main = testArgoRecord

