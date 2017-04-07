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


{-
    <mcp:MD_DataIdentification gco:isoType="gmd:MD_DataIdentification">
      <gmd:citation>
        <gmd:CI_Citation>
          <gmd:title>
            <gco:CharacterString>IMOS - Argo Profiles</gco:CharacterString>
          </gmd:title>
-} 


parseTitle =
  atTag "mcp:MD_DataIdentification" >>>
  proc dataIdent -> do
    title <- atChildName "gmd:citation" >>> atChildName "gmd:CI_Citation" >>> atChildName "gmd:title"  >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent
    abstract <- atChildName "gmd:abstract" >>> atChildName "gco:CharacterString" >>> getChildText -< dataIdent

    returnA -< (title,abstract)

{-

      </gmd:citation>
      <gmd:abstract>
        <gco:CharacterString>Argo Australia aims to undertake real time monitoring of the broad ocean state around Australia by maintaining an array of profiling (Argo) floats that measure temperature, salinity and pressure down to 2000m every 10 days in real time. The data presented here, represent all Australian Argo profiles collected since 2000, and covers the oceans in the southern hemisphere worldwide A typical Argo float mission is to profile from 2000 m depth to the sea surface every 10 days. On deployment, the float sinks to a depth of 1000 m and drifts with the ocean currents for 9 days. Then the float sinks deeper to its profile depth (usually 2000 m) before starting to ascend through the water column measuring temperature, salinity, pressure, and oxygen (on selected floats) as it rises. Once at the surface it transmits location and profile data via satellite to land-based Argo data centres. After transmission the float sinks again and repeats the cycle. Each Argo float is identified by a unique identification number called a WMO ID. WMO (World Meteorological Organisation) ID Numbers are assigned to measurement stations and observing platforms to enable researchers to keep track of, and uniquely identify their floats. The average life of the latest model APEX Argo floats are around 3.7 years or approximately 135 cycles. These statistics are for floats with the standard alkaline battery configuration from an analysis by Kobayashi et al (2009). In the Australian Argo program, the floats are deployed with a combination of lithium and alkaline battery packs which extends float lifetime. Argo Australia floats usually last 5 years and several floats are approaching their 9th birthday and are still returning good data.</gco:CharacterString>
      </gmd:abstract>
 

-}



parseOnlineResources =
  atTag "gmd:CI_OnlineResource" >>>
  proc r -> do
    protocol    <- atChildName "gmd:protocol" >>> atChildName "gco:CharacterString" >>> getChildText  -< r
    linkage     <- atChildName "gmd:linkage"  >>> atChildName "gmd:URL" >>> getChildText -< r
    description <- atChildName "gmd:description" >>> atChildName "gco:CharacterString" >>> getChildText -< r
    returnA -< (protocol, linkage, description)


{-
parseDataParameters' =
  atTag "mcp:dataParameter" >>>
  proc dp -> do
    term <- atChildName "mcp:DP_DataParameter" >>> atChildName "mcp:parameterName" >>> atChildName "mcp:DP_Term" -< dp
    txt  <- atChildName "mcp:term"  >>> atChildName "gco:CharacterString" >>> getChildText -< term
    url  <- atChildName "mcp:vocabularyTermURL"  >>> atChildName "gmd:URL" >>> getChildText -< term
    returnA -< (txt, url)
-}


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

    -- title
    title <- runX (parsed >>> parseTitle )
    print title


main = testArgoRecord 
 
