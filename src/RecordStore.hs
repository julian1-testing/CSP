

module RecordStore where

-- import Helpers(parseXML, atTag, atChildName, getChildText, stripSpace)

import Database.PostgreSQL.Simple as PG(query, execute, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import Text.RawString.QQ


import Record(Record, showRecord)
import ParseMCP20(parse)



----------------

{-
    I think we're basically going to want to redo this....


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



main = do
    print "hi"

    recordText <- readFile "./test-data/argo.xml"
    let elts = Helpers.parseXML recordText

    myRecord <- MetadataRecord.parseMCP20 elts 
    putStrLn $ showRecord myRecord
    return ()




