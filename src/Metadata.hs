{-
  
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}


module Metadata where


-- import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG(Only(..))

import Text.RawString.QQ

-- import qualified Record.DataIdentification as Record(DataIdentification(..))  

import Record

-- THINK we want a proper constructor with the actual values...
-- is left outer join ok? 
-- that will probably take care of the formatting also,

-- ok, I think we may actually want to combine all the data stuctures back together again... 
-- hmmmm, - and if it's one-to-may - then can't actually be done in a single sql statement ...

-- record 120 - this is more complicated than it looks. 

{-
    jurisdictionLink :: String, 
    licenseLink :: String , 
    licenseName :: String , 
    licenseImageLink:: String

    title:: String, 
    abstract:: String



harvest=> g^C
harvest=> select count(1) from md_commons ;
 count
-------
   106
(1 row)

harvest=> select count(1) from record ;
 count
-------
   120
(1 row)

harvest=> select count(1) from data_identification ; 
 count 
-------
   120
(1 row)

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


data MDCommons = MDCommons { 

    jurisdictionLink :: String, 
    licenseLink :: String , 
    licenseName :: String , 
    licenseImageLink:: String

} deriving (Show, Eq)



-}

-- constructing this thing back into a sensible object is not that simple ...

-- VERY IMPORTANT - 
-- I think - we may want to do it object by object.... - that way we can assemble everything 
-- then we can just call sql for each record that we have to process 
-- and not do anything....

-- but if that's the case... - we should compose it - bit by bit according to the record id 


getRecordUuid conn record record_id = do
  xs :: [ (Integer, String)] <- PG.query conn [r|
      select
        record.id,
        uuid
      from record
      where record.id = ?
    |]
    $ Only (record_id :: Integer )
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
   $ Only (record_id :: Integer )
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
   $ Only (record_id :: Integer )
  return $
    case xs of 
      [ (Just jurisdictionLink, Just licenseLink, Just licenseName, Just licenseImageLink) ] 
          -> record { mdCommons = Just $ MDCommons jurisdictionLink licenseLink licenseName licenseImageLink }
      _ -> record





main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  let record_id = 289

  record <- getRecordUuid conn emptyRecord record_id
  -- (putStrLn.show) record 

  record <- getRecordMDCommons conn record record_id 

  -- putStrLn "----"

  record <- getRecordDataIdentification conn record record_id
  (putStrLn.show) record 

  return ()




--  let r = head xs 
--   let record = Record (Just $ snd r) Nothing Nothing [] [] [] Nothing [] []  
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
