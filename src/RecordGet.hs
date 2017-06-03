{-

  - denormalize the Record structure in the db into hasekll records, according to record_id

  - the way to improve performance here (behave more like orm mapper), is to left join all record with the table/data of interest
      then map and appropriate for each record_id on the client side of the db.
-}

{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}


module RecordGet where


import qualified Database.PostgreSQL.Simple as PG(connect, close, query)
import Database.PostgreSQL.Simple.Types(Only(..))
import qualified Data.ByteString.Char8 as BS(ByteString(..) )
import Text.RawString.QQ


import Record
import qualified Config as Config(connectionInfo)




getRecordIdFromUuid conn uuid = do
  -- convenience function.  helper for tests -
  -- TODO this code doesn't really belong here - better place to put this?
  xs :: [ (Only Int)] <- PG.query conn [r|
      select
        record.id
      from record
      where record.uuid = ?
    |]
    $ Only (uuid :: BS.ByteString)
  return $
    -- TODO can we use listToMaybe? - not sure because the Only also needs destructuring
    case xs of
      [ Only record_id ] -> Just record_id
      _ -> Nothing



-- TODO combine this with getRecordUUID and return source, source_id, and uuid

getRecordSource conn record_id = do
  xs :: [ (Only BS.ByteString)] <- PG.query conn [r|
      select
        source
      from source
      join record on record.source_id = source.id
      where record.id = ?
    |]
    $ Only (record_id :: Int)
  return $
    case xs of
      [ (Only source) ] -> Just source
      _ -> Nothing




getRecordUuid conn record_id = do
  xs :: [ (Int, BS.ByteString )] <- PG.query conn [r|
      select
        record.id,
        uuid
        -- add source...
      from record
      where record.id = ?
    |]
    $ Only (record_id :: Int )
  return $
    case xs of
      [ (record_id, uuid ) ] -> Just uuid
      _ -> Nothing



getRecordDataIdentification conn record_id = do
  xs :: [ (Maybe BS.ByteString, Maybe BS.ByteString) ]  <- PG.query conn [r|
      select
        title,
        abstract
      from data_identification
      where record_id = ?
   |]
   $ Only (record_id :: Int )
  return $
    case xs of
      [ (Just title, Just abstract) ] -> Just $ DataIdentification title abstract
      _ -> Nothing



getRecordMDCommons conn record_id = do
  -- TODO - should fields should support nulls as Maybe type here???
  xs :: [ (Maybe BS.ByteString, Maybe BS.ByteString, Maybe BS.ByteString, Maybe BS.ByteString) ]  <- PG.query conn [r|
      select
        jurisdiction_link,
        license_link,
        license_name,
        license_image_link
      from md_commons
      where record_id = ?
   |]
   $ Only (record_id :: Int)
  return $
    case xs of
      [ (Just jurisdictionLink, Just licenseLink, Just licenseName, Just licenseImageLink) ]
          -> Just $ MDCommons jurisdictionLink licenseLink licenseName licenseImageLink
      _ -> Nothing



getRecordDataParameters conn record_id = do
  xs :: [ (BS.ByteString, BS.ByteString, BS.ByteString) ] <- PG.query conn [r|
      select
        -- concept_view.concept_id,
        -- data_parameter.record_id
        concept_view.label,
        concept_view.url,
        root_label_concept_view.root_label
      from data_parameter
      left join root_label_concept_view on root_label_concept_view.concept_id = data_parameter.concept_id
      left join concept_view on concept_view.concept_id = data_parameter.concept_id
      where record_id = ?
   |]
   $ Only (record_id :: Int )
  return $
    map f xs
    where
      f (label, url, rootLabel) = DataParameter { term = label, url = url, rootTerm = rootLabel }



-- this query may have slowed things down...
getRecordGeopolys conn record_id = do
  xs :: [ (Only BS.ByteString ) ] <- PG.query conn [r|
      select
        poly
      from geopoly
      where record_id = ?
   |]
   $ Only (record_id :: Int )
  return $
    map fromOnly xs



getTransferLinks conn record_id = do
  xs :: [ (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString) ] <- PG.query conn [r|
      select
        protocol,
        linkage,
        name,
        description
      from transfer_link
      where record_id = ?
   |]
   $ Only (record_id :: Int )
  return $
    map f xs
    where
      f ( protocol, linkage, name, description ) = TransferLink protocol linkage name description



getRecordAttrConstraints conn record_id = do
  xs :: [ (Only BS.ByteString ) ] <- PG.query conn [r|
      select
        attr
      from attr_constraint
      where record_id = ?
   |]
   $ Only (record_id :: Int )
  return $
    map fromOnly xs



getRecordUseLimitations conn record_id = do
  xs :: [ (Only BS.ByteString ) ] <- PG.query conn [r|
      select
        limitation
      from use_limitation
      where record_id = ?
   |]
   $ Only (record_id :: Int )
  return $
    map fromOnly xs



getRecord conn record_id = do

  -- TODO there's something slow... although maybe the xml formatting,
  uuid <- getRecordUuid conn record_id
  source <- getRecordSource conn record_id
  dataIdentification <- getRecordDataIdentification conn record_id
  mdCommons <- getRecordMDCommons conn record_id
  dataParameters <- getRecordDataParameters conn record_id
  transferLinks <- getTransferLinks conn record_id
  geopolys <- getRecordGeopolys conn record_id
  attrConstraints <- getRecordAttrConstraints conn record_id
  useLimitations <- getRecordUseLimitations conn record_id


  let record = Record {
                uuid = uuid,
                source = source,
                dataIdentification = dataIdentification,
                mdCommons = mdCommons,
                attrConstraints = attrConstraints,
                useLimitations = useLimitations,
                dataParameters = dataParameters,
                temporalBegin = Nothing,
                transferLinks = transferLinks,
                geopoly = geopolys -- TODO should be plural
              }

  -- print record
  return record



getRecords conn records = do
  mapM (getRecord conn) records



----
-- tests

main :: IO ()
main = do
  conn <- PG.connect Config.connectionInfo

  let record_id = 289
  record <- getRecord conn record_id

  -- (putStrLn.show) $ record.uuid
  -- fucking hell
  let b = case (uuid record) of
                  Just a -> ""
                  Nothing -> ""

  (putStrLn.show) $ record

  PG.close conn
