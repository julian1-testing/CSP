
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module ParseMCP20 where

{-
    IMPORTANT must close resources !!!
    responseClose :: Response a -> IO ()
-}


import Text.XML.HXT.Core

import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace)

-- import everything
import Record --as Record(Record(..))


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






parse elts = do
    -- runX returns an IO type...
    -- so this has to monadic
    -- but - is it possible to run a non monadic runX to parse everything?

    identifier <- runX (elts >>> parseFileIdentifier)

    dataIdentification <- runX (elts >>> parseDataIdentification )

    attrConstraints <- runX (elts >>> parseAttributionConstraints)

    useLimitations <- runX (elts >>> parseUseLimitations)

    dataParameters <- runX (elts >>> parseDataParameters)

    temporalBegin <- runX (elts >>> parseTemporalExtentBegin )

    transferLinks <- runX (elts >>> parseTransferLinks)

    geoPoly <- runX (elts >>> parseGeoPolygon )


    let a = case (identifier, dataIdentification, temporalBegin) of
            ([ uuid ], [ di ], [tb]) -> Right $ Record {
                uuid = uuid,
                dataIdentification = di,
                attrConstraints = attrConstraints ,
                useLimitations = useLimitations,
                dataParameters = dataParameters,
                temporalBegin = tb,
                transferLinks = transferLinks,
                geoPoly = geoPoly
            }
            ( _, _, _) -> Left "missing uuid, dataIdentification or temporalBegin"



    return a



testArgoRecord = do

    -- recordText <- readFile "./test-data/aus-cpr.xml"
    recordText <- readFile "./test-data/argo.xml"
    let elts = Helpers.parseXML recordText

    myRecord <- parse elts
    putStrLn $ case myRecord of
        Right myRecord  -> showRecord myRecord
        Left msg -> msg

    return ()


main = testArgoRecord



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

