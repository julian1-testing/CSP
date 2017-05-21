{-
    module for Storing a Record to db

-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RecordStore where


import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import Text.RawString.QQ

import Helpers as H -- (parseXML)
import ParseMCP20(parse)
import Record


----------------

{-
    ok - get rid of the title,

    -- should be upsert - and probably the only one.
    -- and return the id
-}


storeUUID conn uuid = do


    -- must get the id, so we can delete all old bits,
    -- this is harder than it looks....
    -- insert or select item, returning id
    -- http://stackoverflow.com/questions/18192570/insert-if-not-exists-else-return-id-in-postgresql

    xs :: [ (Only Int)] <- PG.query conn
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





storeMDCommons conn record_id mdCommons = do
    print "hi this is mdCommons"
    {- deleting each time isn't nearly as nice
    -- upsert....
    -- but if there are multiple items......  then we have to do a delete first... in case one has been removed????
    -- Note that we can insert the last modified time in the on conflict clause which is quite nice.
    -}

    xs :: [ (Only Int)] <- PG.query conn
        [r|
            -- using upsert
            -- we use cte in order not to pass the arguments more than once....
            with a as (
                select
                ? as record_id,
                ? as jurisdiction_link,
                ? as license_link,
                ? as license_name,
                ? as license_image_link
            )
            insert into md_commons(
                record_id,
                jurisdiction_link,
                license_link,
                license_name,
                license_image_link
            )
            (select * from a)
            on conflict (record_id)
            do update set
                jurisdiction_link = (select jurisdiction_link from a),
                license_link = (select license_link from a),
                license_name = (select license_name from a),
                license_image_link = (select license_image_link from a)
            returning md_commons.id
        |]
        -- there's a limit of 9 elements in the tuple....
        (   record_id :: Int,
            jurisdictionLink mdCommons,-- :: String, --ckk
            licenseLink mdCommons,-- :: String,
            licenseName mdCommons,-- :: String,
            licenseImageLink mdCommons--  :: String
        )
    return ()





storeDataIdentification conn record_id dataIdentification = do
    print "hi this is dataIdentification"
    {- deleting each time isn't nearly as nice
    -- upsert....
    -- but if there are multiple items......  then we have to do a delete first... in case one has been removed????
    -- Note that we can insert the last modified time in the on conflict clause which is quite nice.
    -}
    -- cvould be null

    xs :: [ (Only Int)] <- PG.query conn
        [r|
            -- using upsert
            -- we use cte in order not to pass the arguments more than once....
            with a as (
                select
                ? as record_id,
                ? as title,
                ? as abstract
            )
            insert into data_identification(
                record_id,
                title,
                abstract
            )
            (select * from a)
            on conflict (record_id)
            do update set
                title = (select title from a),
                abstract = (select abstract from a)
            returning data_identification.id
        |]
        -- there's a limit of 9 elements in the tuple....
        $ let d = dataIdentification in
        (   record_id :: Int,
            title dataIdentification,-- :: String,
            abstract dataIdentification-- :: String,
        )
    return ()




storeTransferLink conn record_id transferLink = do

    xs :: [ (Only Int)] <- PG.query conn
        [r|
            with a as (
                select
                ? as record_id,
                ? as protocol,
                ? as linkage,
                ? as description
            )
            insert into transfer_link (
                record_id,
                protocol,
                linkage,
                description
            )
            (select * from a)
            on conflict(record_id, protocol, linkage) -- (my_transfer_protocol_unique_idx )
            do update set
                protocol = (select protocol from a),
                linkage = (select linkage from a),
                description = (select description from a)

            returning transfer_link.id
        |]
        $ let t = transferLink in
        (
            record_id, -- :: Int,
            protocol t,-- :: String,
            linkage t, -- :: String,
            description t-- :: String
        )
    return ()


storeTransferLinks conn record_id transferLinks = do
    -- mapM (putStrLn.show) transferLinks
    mapM (storeTransferLink conn record_id) transferLinks




------------
-- dataParameter


storeDataParameter conn record_id dataParameter  = do
{-
    Important - vocab must be loaded for indexing parameters!!!!
-}
    let url_ = url dataParameter

    -- look up the required concept
    -- use separate sql statements to make it easier to write stdout/log
    xs :: [ (Int, String) ] <- PG.query conn [r|
        select id, label 
        from concept where url = ?
        |]
        (Only url_)

    -- putStrLn $ (show.length) xs

    -- make sure we found the concept
    case length xs of
      1 -> do
        -- store the concept
        let (concept_id, concept_label) : _ = xs
        PG.execute conn [r|
          insert into data_parameter(concept_id, record_id)
          values (?, ?)
            -- the same parameter gets repeated in the record because of poor modelling
          on conflict
          do nothing
        |] (concept_id :: Int, record_id:: Int)
        return ()

      0 -> putStrLn $ "dataParameter '" ++ url_ ++ "' not found!"
      _ -> putStrLn $ "dataParameter '" ++ url_ ++ "' found multiple matches?"



storeDataParameters conn record_id dataParameters = do
    -- delete existing data parameters associated with record_id first, so that
    -- if a item is removed from record it will be removed here
    -- it might be nicer to do upsert - but need more complicated mechanism - to 
    -- track removals...
    PG.execute conn [r|
        delete from data_parameter
        where record_id = ?
        |]
        (Only record_id)

    mapM (storeDataParameter conn record_id) dataParameters



------------
-- attrConstraints


storeAttrConstraints conn record_id attrConstraints = do
    PG.execute conn [r|
        delete from attr_constraint
        where record_id = ?
        |] (Only record_id)

    mapM (storeAttrConstraint conn record_id) attrConstraints
    where
    storeAttrConstraint conn record_id attrConstraint = do
        -- this should be normalized ... eg. find the text and just rejoin
        -- VERY IMPORTANT this solves the problem, of making the data static... 
        -- each time harvest we just create new mapping table rows
        -- it's going to be an upsert...
      
        PG.execute conn [r|
            insert into attr_constraint(record_id, attr)
            values (?, ?)
            |] (record_id, attrConstraint)


------------
-- useLimitations

storeUseLimitations conn record_id useLimitations = do
    PG.execute conn "delete from use_limitation where record_id = ?" (Only record_id)
    mapM (store conn record_id) useLimitations
    where
    store conn record_id useLimitation = do
        PG.execute conn "insert into use_limitation(record_id, limitation) values (?, ?)" 
          (record_id, useLimitation)

------------
-- temporalBegin

storeTemporalBegin conn record_id temporalBegin = do
    PG.execute conn "delete from temporal_begin where record_id = ?" (Only record_id)
    PG.execute conn "insert into temporal_begin(record_id, begin_) values (?, ?)" (record_id, temporalBegin)
    return ()
 
------------
-- storeGeoPoly

storeGeopolys conn record_id geopolys = do
    PG.execute conn "delete from geopoly where record_id = ?" (Only record_id)
    mapM (store conn record_id) geopolys
    where
    store conn record_id geopoly = do
        PG.execute conn "insert into geopoly(record_id, poly) values (?, ?)" 
          (record_id, geopoly)





storeAll conn record = do

    case (Record.uuid record) of 
            Just uuid_ -> do

                -- TODO need to do this entire thing transactionally,

                -- change name storeOrGetUUID
                -- if we can't get the recordId we're really stuck
                record_id <- storeUUID conn uuid_ 

                -- https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/base-4.9.0.0/Data-Maybe.html
                maybe (return ()) (storeDataIdentification conn record_id) (Record.dataIdentification record) 
                maybe (return ()) (storeMDCommons conn record_id) (Record.mdCommons record) 

                storeTransferLinks conn record_id (Record.transferLinks record)
                storeDataParameters conn record_id (Record.dataParameters record)

                -- need normalizing
                storeAttrConstraints conn record_id (Record.attrConstraints record)
                storeUseLimitations conn record_id (Record.useLimitations record)

                --
                maybe (return ()) (storeTemporalBegin conn record_id) (Record.temporalBegin record) 

                storeGeopolys conn record_id (Record.geopoly record)

                return ()
            _ ->
                -- unlikely - but there's nothign we can do except log to stdout...
                print "error -> No uuid in record????"



deleteAll conn = do
    -- rename deleteAllRecords
    -- pretty useful for testing - leaves vocab
    PG.execute conn [r|
        truncate record, transfer_link, data_parameter, data_identification, 
        md_commons, attr_constraint, use_limitation, temporal_begin,
        geopoly
        ;
    |] ()

    return ()



main = do
    print "hi"

    conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

    recordText <- readFile "./test-data/argo.xml"
    let elts = parseXML recordText
    record <- ParseMCP20.parse elts
    print record
    storeAll conn record

    PG.close conn



{-
            -- storeUUID conn uuid title = do
            record_id <- storeUUID conn 
            storeDataIdentification conn record_id (Record.dataIdentification record)
            storeTransferLinks conn record_id (Record.transferLinks record)
-}
{- 
    -- look up the required concept
    -- think we want to do this separately 
    xs :: [ (Int, String) ] <- query conn "select id, label from concept where url = ?" (Only url)
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
        |] (concept_id :: Int, uuid :: String)
        return ()

      0 -> putStrLn $ "dataParameter '" ++ url ++ "' not found!"
      _ -> putStrLn $ "dataParameter '" ++ url ++ "' found multiple matches?"

-}
    -- dataParameters <- runX (parseXML recordText >>> parseDataParameters)
    -- putStrLn $ "data parameter count: " ++ (show.length) dataParameters
    -- mapM (putStrLn.show) dataParameters

