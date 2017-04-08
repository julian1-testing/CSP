
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RecordStore where


import Database.PostgreSQL.Simple as PG(query, execute, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import Text.RawString.QQ

import Helpers(parseXML)
import Record(Record, DataIdentification(title), showRecord, dataIdentification)

-- import Record.DataIdentification(title)

import ParseMCP20(parse)


----------------

{-
    ok - get rid of the title,

    -- should be upsert - and probably the only one.
    -- and return the id
-}


processRecordUUID conn uuid = do
    -- must get the id, so we can delete all old bits,
    -- this is harder than it looks....
    -- insert or select item, returning id
    -- http://stackoverflow.com/questions/18192570/insert-if-not-exists-else-return-id-in-postgresql

    xs :: [ (Only Integer)] <- PG.query conn
        [r|
            with s as (
                select id
                from record
                where uuid = ?
            ),
            i as (
                insert into record(uuid)
                select ?
                where not exists (select 1 from s)
                returning id
            )
            select id from i
            union all
            select id from s
        |]
        (uuid :: String, uuid :: String)


    let record_id = case xs of
         [] -> -99999 -- avoided because sql will return a value
         [ Only record_id ] -> record_id


    putStrLn $ "record_id is " ++ show record_id
    return record_id



processDataIdentification conn record_id dataIdentification = do
    print "hi this is dataIdentification"

    xs :: [ (Only Integer)] <- PG.query conn
        [r|
            insert into data_identification( 
                record_id,
                title
            )
            values (?, ?)
            returning id
        |]
        (record_id :: Integer, 
         title dataIdentification :: String
        )

    return ()




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

    myRecord <- ParseMCP20.parse elts
    putStrLn $ showRecord myRecord

    -- processRecordUUID conn uuid title = do

    conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

    record_id <- processRecordUUID conn "myuuid" 

    processDataIdentification conn record_id (Record.dataIdentification myRecord)

    return ()




