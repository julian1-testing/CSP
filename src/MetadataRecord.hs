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
        
        organisationField    - **** don't have yet - .

        jurisdictionLink    done
        licenseLink         done
        licenseName         done
        licenseImagelink    done

        attrConstrField -> attrConstr ->  
            - more than one in MD_Commons 

        otherConstrField ->  also in MDcommons

        otherCitation  <- does not appear in summary-response
        useLimitationiField -> useLimitation - more than one

-}

parseFileIdentifier = 
  atTag "gmd:fileIdentifier" >>>
  proc identifier -> do
    uuid <- atChildName "gco:CharacterString" >>> getChildText -< identifier
    returnA -< (uuid)


data MCP2Record = MCP2Record { title:: String  
                     , abstract:: String  
                     , jurisdictionLink :: String
                     , licenseLink :: String
                     , licenseName :: String
                     , licenseImageLink:: String
--                     , attrConstr :: String
--                     , otherAttrConstr :: String
                     } deriving (Show, Eq)  

-- p = Person { firstName = "john", lastName = "madden", age = 34 }  



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
 
    returnA -< MCP2Record { 
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

{-
      <gmd:resourceConstraints>
        <gmd:MD_Constraints>
          <gmd:useLimitation>
            <gco:CharacterString>Data, products and services from IMOS are provided "as is" without any warranty as to fitness for a particular purpose.</gco:CharacterString>
 
-}

parseUseLimitations =
  -- have more than one
  atTag "gmd:resourceConstraints"  >>> atChildName "gmd:MD_Constraints" >>>
  proc md_commons -> do
    useLimitation <- atChildName "gmd:useLimitation" >>> atChildName "gco:CharacterString" >>> getChildText -< md_commons
    returnA -< useLimitation





    {-
        -- no  
        -- gmd:MD_Constraints
        md_constraints <- atChildName "gmd:resourceConstraints" >>> atChildName "gmd:MD_Constraints" -< dataIdent 

        otherAttrConstr <- atChildName "gmd:useLimitation" >>> atChildName "gco:CharacterString" >>> getChildText -< md_constraints
    -}
    -- it's getting hard to read see this....




--- OK 
 -- where getting multiple things????

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


    -- dataIdentification
    print "###### data identification stuff"
    x <- runX (parsed >>> parseDataIdentification )
    print x 

    -- dataIdentification
    print "###### attribution constraints "
    y <- runX (parsed >>> parseAttributionConstraints)
    mapM print y

    print "###### useLimitations"
    j <- runX (parsed >>> parseUseLimitations)
    mapM print j

    return ()


main = testArgoRecord 
 
