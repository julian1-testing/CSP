{-

  denormalize the Record structure in the db into hasekll records, according to record_id 

-}

-- needed for disambiguating types,
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}


module RecordGet where


import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import qualified Data.ByteString.Char8 as BS(ByteString(..) )
import Text.RawString.QQ

import Record



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
      f ( protocol, linkage, name description ) = TransferLink protocol linkage name description 



getRecord conn record_id = do

  -- TODO there's something slow... although maybe the xml formatting,
  uuid <- getRecordUuid conn record_id
  dataIdentification <- getRecordDataIdentification conn record_id
  mdCommons <- getRecordMDCommons conn record_id
  dataParameters <- getRecordDataParameters conn record_id
  transferLinks <- getTransferLinks conn record_id
  geopolys <- getRecordGeopolys conn record_id

  let record = Record { 
                uuid = uuid, 
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



main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  let record_id = 289
  record <- getRecord conn record_id

  -- (putStrLn.show) $ record.uuid
--- fucking helll
  let b = case (uuid record) of
                  Just a -> ""
                  Nothing -> ""


  (putStrLn.show) $ record





-- THINK we want a proper constructor with the actual values...
-- is left outer join ok?
-- that will probably take care of the formatting also,

-- ok, I think we may actually want to combine all the data stuctures back together again...
-- hmmmm, - and if it's one-to-may - then can't actually be done in a single sql statement ...

-- record 120 - this is more complicated than it looks.

{-
data Record = Record {

    uuid :: Maybe String,
    dataIdentification :: Maybe DataIdentification ,
    mdCommons :: Maybe MDCommons,
    attrConstraints :: [ String ],   -- todo
    useLimitations :: [ String ],    -- todo
    dataParameters :: [ DataParameter ],
    temporalBegin :: Maybe String,   -- todo
    transferLinks :: [ TransferLink ],
    geopoly :: [ String ]            -- todo
} deriving (Show, Eq)


-}

-- constructing this thing back into a sensible object is not that simple ...

-- VERY IMPORTANT -
-- I think - we may want to do it object by object.... - that way we can assemble everything
-- then we can just call sql for each record that we have to process
-- and not do anything....

-- but if that's the case... - we should compose it - bit by bit according to the record id


---- hmmmm this is a lot of work...
---- we have RecordStore RecordRetrieve


{-
  record <- getRecordUuid conn emptyRecord record_id
  -- (putStrLn.show) record
  record <- getRecordMDCommons conn record record_id
  -- putStrLn "----"
  record <- getRecordDataIdentification conn record record_id
-}


-- let r = head xs
-- let record = Record (Just $ snd r) Nothing Nothing [] [] [] Nothing [] []
-- mapM (putStrLn.show) xs
-- return xs

{-
  let f (\id uuid title abstract ->  Record uuid DataIdentificatoin title abstract
      []
      []
      []
      None
      []
      []
    )
-}
 {-
  let di = Just $ DataIdentification "ssss" "ppppp"
  let c = MDCommons "a" "b" "c" "d"
  let record = Record (Just "uuid") di (Just c)  [] [] [] Nothing [] []
  let xs' = map id xs
-}
