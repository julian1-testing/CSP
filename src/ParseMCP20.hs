
{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}


module ParseMCP20 where

{-
    IMPORTANT must close resources !!!

	not sure if have closed HXT resources...
	but we're only doing a string?
-}


import Control.Exception
import Text.XML.HXT.Core
import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace)
import qualified Data.ByteString.Char8 as BS(ByteString(..), pack )
import qualified Data.Maybe as Maybe(listToMaybe)

-- import everything
import Record


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
  -- multiple
  atTag "gmd:extent"  >>> atChildName "gmd:EX_Extent" >>>
  proc extent -> do
    begin <- atChildName "gmd:geographicElement" >>> atChildName "gmd:EX_BoundingPolygon"
        >>> atChildName "gmd:polygon" >>> atChildName "gml:Polygon"
        >>> atChildName "gml:exterior" >>> atChildName "gml:LinearRing"
        >>> atChildName "gml:posList" >>> getChildText -< extent
    returnA -< BS.pack begin



parseTransferLinks =
  atTag "gmd:transferOptions" >>> atChildName "gmd:MD_DigitalTransferOptions" >>>
  proc transfer -> do

    resource <- atChildName "gmd:onLine" >>> atChildName "gmd:CI_OnlineResource" -<  transfer

    protocol    <- atChildName "gmd:protocol" >>> atChildName "gco:CharacterString" >>> getChildText  -< resource
    linkage     <- atChildName "gmd:linkage"  >>> atChildName "gmd:URL" >>> getChildText -< resource
    -- https://stackoverflow.com/questions/34694801/hxt-using-orelse-to-replace-missing-attribute-value-with-default 
    name        <- (atChildName "gmd:name" >>> atChildName "gco:CharacterString" >>> getChildren >>> isText >>> getText ) `orElse` ( constA "" ) -< resource 
    description <- atChildName "gmd:description" >>> atChildName "gco:CharacterString" >>> getChildText   -< resource 

    returnA -< TransferLink {
        protocol = BS.pack protocol,
        linkage = BS.pack linkage,
        name = BS.pack name,
        description = BS.pack description
    }


parseDataParameters =
  -- multiple??
  atTag "mcp:DP_Term" >>>
  proc term -> do
    term_ <- atChildName "mcp:term"  >>> atChildName "gco:CharacterString" >>> getChildText -< term
    url  <- atChildName "mcp:vocabularyTermURL"  >>> atChildName "gmd:URL" >>> getChildText -< term
    returnA -< DataParameter {
        term = BS.pack term_,
        url = BS.pack url,
        rootTerm = BS.pack "" -- NOTE empty on parse, but filled in when we return.
    }




------------------
-- tests


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
    geopoly <- runX (elts >>> parseGeoPolygon )

    -- print $ "identifier " ++ show identifier
    -- print $ "dataIdentification " ++ show dataIdentification
    -- print $ "mdCommons " ++ show mdCommons
    -- print $ "temporalBegin " ++ show temporalBegin

    -- temporal begin is sometimes empty...

    -- TODO use Data-Maybe  . listToMaybe 
{-
    let uuid' = case identifier of
            [ id ] -> Just id
            _ -> Nothing
    let dataIdentification' = case dataIdentification of
         [ di ] -> Just di
         _ -> Nothing
    let mdCommons' = case mdCommons of
         [ mdc ] -> Just mdc
         _ -> Nothing

    let temporalBegin' = case temporalBegin of
         [ tb ] -> Just tb
         _ -> Nothing

-}

    let uuid' = Maybe.listToMaybe identifier
    let dataIdentification' = Maybe.listToMaybe dataIdentification
    let mdCommons' = Maybe.listToMaybe mdCommons
    let temporalBegin' = Maybe.listToMaybe temporalBegin

    let record = Record {
      uuid = uuid',
      dataIdentification = dataIdentification',
      mdCommons = mdCommons',
      attrConstraints = attrConstraints ,
      useLimitations = useLimitations,
      dataParameters = dataParameters,
      temporalBegin = temporalBegin',
      transferLinks = transferLinks,
      geopoly = geopoly
      }

    return record




testArgoRecord = do

    -- recordText <- readFile "./test-data/aus-cpr.xml"
    recordText <- readFile "./test-data/argo.xml"
    let elts = Helpers.parseXML recordText

    record <- parse elts

    print $ show record

    print "number of transferLinks"
    print $ length $ transferLinks record

    return ()


main = testArgoRecord





