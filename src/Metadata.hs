{-
  
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}


module Metadata where


import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
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


getRecordList conn = do

  let query1 = [r|
      select
        record.id,
        uuid,

        di.title,
        di.abstract,

        c.jurisdiction_link,
        c.license_link,
        c.license_name,
        c.license_image_link

      from record

      left join data_identification di on di.record_id = record.id 
      left join md_commons c on c.record_id = record.id 
      -- left join data_parameter on data_parameter.concept_id = concept_view.concept_id
      -- left join record on data_parameter.record_id = record.id
  |]
  xs :: [ 
    (Integer, -- record_id 
    String,   -- uuid 

    String, String,  -- data identfication
    Maybe String, Maybe String, Maybe String, Maybe String    -- md commons

    ) ] <- PG.query conn query1 ()
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
  
  let di = Just $ DataIdentification "ssss" "ppppp"
  let c = MDCommons "a" "b" "c" "d" 
  let record = Record (Just "uuid") di (Just c)  [] [] [] Nothing [] []  

  let xs' = map id xs 

  mapM (putStrLn.show) xs
  return xs



main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  getRecordList conn
  return ()


