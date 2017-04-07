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

-- we don't parse the uuid - because we have required it...


-- need a data structure...
-- should parse the identifier - even if it's not used for CSW 

{-
    ./web-app/js/portal/data/MetadataRecord.js
        title
        abstract
        uuid
        parameterField   - db join vocab
        platform         - db field join vocab
        
        organisation    - **** don't have yet - .

        jurisdictionLink 
      <gmd:resourceConstraints>
        <mcp:MD_Commons gco:isoType="gmd:MD_Constraints" mcp:commonsType="Creative Commons">
          <mcp:jurisdictionLink>
            <gmd:URL>http://creativecommons.org/international/</gmd:URL>
          </mcp:jurisdictionLink>
 
        

    parameters we have - just join in the db and format
    organisation we have - just formatting...

-}

parseFileIdentifier = 
  atTag "gmd:fileIdentifier" >>>
  proc identifier -> do
    uuid <- atChildName "gco:CharacterString" >>> getChildText -< identifier
    returnA -< (uuid)



parseDataIdentification =
  atTag "mcp:MD_DataIdentification" >>>
  proc dataIdent -> do

    title <- atChildName "gmd:citation" >>> atChildName "gmd:CI_Citation" >>> atChildName "gmd:title"  
        >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent

    abstract <- atChildName "gmd:abstract" >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent

    -- jurisdictionLink <- atChildName "gmd:abstract" >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent

    -- jurisdictionLink <- atChildName "gmd:resourceConstraints" >>> atChildName "mcp:MD_Commons" 
        -- >>> getChildText -< dataIdent

    jurisdictionLink <- atChildName "gmd:resourceConstraints" >>> atChildName "mcp:MD_Commons" >>> atChildName "mcp:jurisdictionLink" >>> atChildName "gmd:URL" >>> getChildText -< dataIdent


    returnA -< (title,abstract, jurisdictionLink)


parseJurisdictionLink =
  atTag "mcp:MD_DataIdentification" >>>
  proc dataIdent -> do
    title <- atChildName "gmd:citation" >>> atChildName "gmd:CI_Citation" >>> atChildName "gmd:title"  
        >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent
    abstract <- atChildName "gmd:abstract" >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent
    returnA -< (title,abstract)




parseOnlineResources =
  atTag "gmd:CI_OnlineResource" >>>
  proc r -> do
    protocol    <- atChildName "gmd:protocol" >>> atChildName "gco:CharacterString" >>> getChildText  -< r
    linkage     <- atChildName "gmd:linkage"  >>> atChildName "gmd:URL" >>> getChildText -< r
    description <- atChildName "gmd:description" >>> atChildName "gco:CharacterString" >>> getChildText -< r
    returnA -< (protocol, linkage, description)




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

processOnlineResource conn uuid (protocol,linkage, description) = do
    PG.execute conn [r|
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




testArgoRecord = do
    recordText <- readFile "./test-data/argo.xml" 

    let parsed = Helpers.parseXML recordText 

    -- data parameters
    dataParameters <- runX (parsed >>> parseDataParameters)
    mapM print dataParameters

    -- identifier
    identifier <- runX (parsed >>> parseFileIdentifier)
    print identifier




    -- title
    x <- runX (parsed >>> parseDataIdentification )
    let (title, abstract, jurisdictionLink) = head x

    print title 
    print abstract
    print jurisdictionLink

    return ()


main = testArgoRecord 
 
