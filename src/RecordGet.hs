{-
  
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}


module RecordGet where


-- import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG(Only(..))

import Text.RawString.QQ

-- import qualified Record.DataIdentification as Record(DataIdentification(..))  

import Record



getRecordUuid conn record record_id = do
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
      [ (record_id, uuid ) ] -> record { uuid = Just uuid } 
      _ -> record



getRecordDataIdentification conn record record_id = do
  xs ::  [ (Maybe String, Maybe String) ]  <- PG.query conn [r|
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
      [ (Just title, Just abstract) ] -> record { dataIdentification = Just $ DataIdentification title abstract } 
      _ -> record



getRecordMDCommons conn record record_id = do
  xs ::  [ (Maybe String, Maybe String, Maybe String, Maybe String) ]  <- PG.query conn [r|
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
          -> record { mdCommons = Just $ MDCommons jurisdictionLink licenseLink licenseName licenseImageLink }
      _ -> record




getRecordDataParameters conn record record_id = do
  xs ::  [ (String, String, String) ]  <- PG.query conn [r|
      select 
        -- concept_view.concept_id,
        -- data_parameter.record_id
        concept_view.label,
        concept_view.url,
        root_label_concept_view.root_label
      from data_parameter
      left join root_label_concept_view on root_label_concept_view.concept_id = data_parameter.concept_id
      left join concept_view on concept_view.concept_id = data_parameter.concept_id 
      where record_id =  289
   |]
   $ Only (record_id :: Int )
  -- ok this is 
  -- this is more complicated because there will be lots of them....
  -- that we have to consume...
  return $
{-
    term :: String,
    url :: String,
    rootTerm :: String -- this is a bit more expensive to compute
-}
    -- there's not even going to be a match here... - because we just map xs 
    case xs of 
      [ (label, url, rootLabel ) ] 
          -> record { dataParameters =  [ DataParameter { term = label, url = url, rootTerm = rootLabel } ] }

      _ -> record





-- dataParameters ....
-- should probabaly be typles

getRecord conn record_id = do
  let record = emptyRecord
  record <- getRecordUuid conn record record_id
  record <- getRecordMDCommons conn record record_id 
  record <- getRecordDataIdentification conn record record_id


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
