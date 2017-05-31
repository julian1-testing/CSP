{-

  - denormalize the Record structure in the db into hasekll records, according to record_id

  - the way to improve performance here (behave more like orm mapper), is to left join all record with the table/data of interest
      then map and appropriate for each record_id on the client side of the db.
-}

{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}


module RecordGet where


import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import qualified Data.ByteString.Char8 as BS(ByteString(..) )
import Text.RawString.QQ

import qualified Config as Config(connString)

import Record




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







-- TODO uuid and source should be combined in get accessor...
-- maybe... it is a join

getRecordSource conn record_id = do
  xs :: [ (Only String)] <- PG.query conn [r|
      select
        source
      from source
      left join record on record.source_id = source.id
      where record.id = ?
    |]
    $ Only (record_id :: Int)
  return $
    case xs of
      [ (Only source) ] -> Just source
      _ -> Nothing




getRecordUuid conn record_id = do
  xs :: [ (Int, String)] <- PG.query conn [r|
      select
        record.id,
        uuid
      from record
      where record.id = ?
    |]
    $ Only (record_id :: Int )
  return $
    case xs of
      [ (record_id, uuid ) ] -> Just uuid
      _ -> Nothing



getRecordDataIdentification conn record_id = do
  xs :: [ (Maybe String, Maybe String) ]  <- PG.query conn [r|
      select
        di.title,
        di.abstract
      from record
      left join data_identification di on di.record_id = record.id
      where record.id = ?
   |]
   $ Only (record_id :: Int )
  return $
    case xs of
      [ (Just title, Just abstract) ] -> Just $ DataIdentification title abstract
      _ -> Nothing



getRecordMDCommons conn record_id = do
  xs :: [ (Maybe String, Maybe String, Maybe String, Maybe String) ]  <- PG.query conn [r|
      select
        md.jurisdiction_link,
        md.license_link,
        md.license_name,
        md.license_image_link
      from record
      left join md_commons md on md.record_id = record.id
      where record.id = ?
   |]
   $ Only (record_id :: Int )
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
    map (\(Only poly) -> poly ) xs



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



getRecord conn record_id = do

  -- TODO there's something slow... although maybe the xml formatting,
  uuid <- getRecordUuid conn record_id
  source <- getRecordSource conn record_id
  dataIdentification <- getRecordDataIdentification conn record_id
  mdCommons <- getRecordMDCommons conn record_id
  dataParameters <- getRecordDataParameters conn record_id
  transferLinks <- getTransferLinks conn record_id
  geopolys <- getRecordGeopolys conn record_id

  let record = Record {
                uuid = uuid,
                source = source,
                dataIdentification = dataIdentification,
                mdCommons = mdCommons,
                attrConstraints = [],
                useLimitations = [],
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
  conn <- PG.connectPostgreSQL Config.connString

  let record_id = 289
  record <- getRecord conn record_id

  -- (putStrLn.show) $ record.uuid
  -- fucking hell
  let b = case (uuid record) of
                  Just a -> ""
                  Nothing -> ""

  (putStrLn.show) $ record


