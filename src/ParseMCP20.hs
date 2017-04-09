
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module ParseMCP20 where

{-
    IMPORTANT must close resources !!!
    responseClose :: Response a -> IO ()
-}


import Control.Exception 

import Text.XML.HXT.Core

import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace)

-- import everything
import Record --as Record(Record(..))


parseFileIdentifier =
  atTag "gmd:fileIdentifier" >>>
  proc identifier -> do
    uuid <- atChildName "gco:CharacterString" >>> getChildText -< identifier
    returnA -< (uuid)



parseMDCommons =
  atTag "gmd:resourceConstraints" >>> atChildName "mcp:MD_Commons" >>> 
  proc md_commons -> do

    jurisdictionLink <- atChildName "mcp:jurisdictionLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons

    licenseLink <- atChildName "mcp:licenseLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons

    licenseName <- atChildName "mcp:licenseName" >>> atChildName "gco:CharacterString" >>> getChildText -< md_commons

    licenseImageLink <- atChildName "mcp:imageLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons

    returnA -< MDCommons {
        jurisdictionLink = jurisdictionLink,
        licenseLink = licenseLink,
        licenseName = licenseName,
        licenseImageLink = licenseImageLink
        }





parseDataIdentification =
  -- single instance
  atTag "mcp:MD_DataIdentification" >>>
  proc dataIdent -> do

    title <- atChildName "gmd:citation" >>> atChildName "gmd:CI_Citation" >>> atChildName "gmd:title"
        >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent

    abstract <- atChildName "gmd:abstract" >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent

    {-
    -- and only one md_commons
    md_commons <- atChildName "gmd:resourceConstraints" >>> atChildName "mcp:MD_Commons" -< dataIdent
    -- no md commons...

    returnA -< md_commons

    jurisdictionLink <- atChildName "mcp:jurisdictionLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons
    licenseLink <- atChildName "mcp:licenseLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons
    licenseName <- atChildName "mcp:licenseName" >>> atChildName "gco:CharacterString" >>> getChildText -< md_commons
    licenseImageLink <- atChildName "mcp:imageLink" >>> atChildName "gmd:URL" >>> getChildText -< md_commons
-}

    -- maybe change name to DataIdentification
    returnA -< DataIdentification {
        title = title,
        abstract = abstract
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
  -- multiple??
  atTag "mcp:DP_Term" >>>
  proc term -> do
    term_ <- atChildName "mcp:term"  >>> atChildName "gco:CharacterString" >>> getChildText -< term
    url  <- atChildName "mcp:vocabularyTermURL"  >>> atChildName "gmd:URL" >>> getChildText -< term
    returnA -< DataParameter {
        term = term_,
        url = url
    }




----------------



testParse elts = do
    identifier <- runX (elts >>> parseFileIdentifier)
    dataIdentification <- runX (elts >>> parseDataIdentification )
    return dataIdentification



parse elts = do
    -- runX returns an IO type...  so this function must be monadic

    identifier <- runX (elts >>> parseFileIdentifier)
    dataIdentification <- runX (elts >>> parseDataIdentification )
    mdCommons <- runX (elts >>> parseMDCommons)
    attrConstraints <- runX (elts >>> parseAttributionConstraints)
    useLimitations <- runX (elts >>> parseUseLimitations)
    dataParameters <- runX (elts >>> parseDataParameters)
    temporalBegin <- runX (elts >>> parseTemporalExtentBegin )
    transferLinks <- runX (elts >>> parseTransferLinks)
    geoPoly <- runX (elts >>> parseGeoPolygon )

    -- print $ "identifier " ++ show identifier
    -- print $ "dataIdentification " ++ show dataIdentification
    -- print $ "mdCommons " ++ show mdCommons
    -- print $ "temporalBegin " ++ show temporalBegin

    -- temporal begining is empty...

    let uuid' = case identifier of
            [ id ] -> Just id
            _ -> Nothing

    let dataIdentification' = case dataIdentification of
         [ di ] -> Just di
         _ -> Nothing

    let mdCommons' = case mdCommons of
         [ mcd ] -> Just mcd
         _ -> Nothing

    let temporalBegin' = case temporalBegin of
         [ tb ] -> Just tb  
         _ -> Nothing

	-- if the thing doesn't have a uuid it's not recognisable
	-- but we should still continue parsing the data.

    let record = Record {
			uuid = uuid', --identifier, --"myident",--uuid',
			dataIdentification = dataIdentification', -----
			mdCommons = mdCommons', -----
			attrConstraints = attrConstraints ,
			useLimitations = useLimitations,
			dataParameters = dataParameters,
			temporalBegin = temporalBegin',    ----
			transferLinks = transferLinks,
			geoPoly = geoPoly
			}

    return record




testArgoRecord = do

    -- recordText <- readFile "./test-data/aus-cpr.xml"
    recordText <- readFile "./test-data/argo.xml"
    let elts = Helpers.parseXML recordText

    -- ok there must be other stuff it's missing...

    myRecord <- parse elts

    print $ show  myRecord


    return ()


main = testArgoRecord





    -- so how do we handle all this... 
    -- I think we need to change the parsing structure....
    -- WE need null
    
    -- we either use null.... or encode it something else....

--    let ide

-- ok if everything
-- parsing everything optionally - means we don't really have to return left and right 
-- we can analyze it separately later?
-- having the record be sufficiently flexible is quite nice...

-- Maybe have another structure to map from MCP2 to the Record. for other strategies

-- if it has an mdcommons we expect everything in mdcommons

-- VERY IMPORTANT - we should parse it in the same structure that it exists - and handle
-- everything else later.


    -- we really need to have this....
    -- but maybe we can handle another way....
    -- blah blah 
    -- might be easier to just set error?
-- case (identifier, dataIdentification, temporalBegin) of
            -- ([ uuid ], [ di ], [tb]) -> 



{- 
    putStrLn $ case myRecord of
        Right myRecord  -> showRecord myRecord
        Left msg -> msg
 -}   

    -- try ( print $ Left "whoot" ) -- putStrLn.showRecord $ myRecord)
    -- catch (print $ head []) $ \(e ::  Exception NoMethodError) -> print "good message"


{-
   let headWithDefault x d = case not $ null x of
            True -> head x
            False -> d


    let myRecord = Record {
        -- if the list is empty??????   need to test....
        uuid = headWithDefault identifier "blah",
        dataIdentification = head dataIdentification,     -- t dangerous... fixme  uuid should never be null, since searchable index

        attrConstraints = attrConstraints ,
        useLimitations = useLimitations,
        dataParameters = dataParameters,

        temporalBegin = headWithDefault temporalBegin "unknown",
        transferLinks = transferLinks,
        geoPoly = geoPoly
    }
-}

