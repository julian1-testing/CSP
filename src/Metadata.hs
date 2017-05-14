{-
  
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}


module Metadata where


import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Text.RawString.QQ

-- THINK we want a proper constructor with the actual values...


getRecordList conn = do

  let query1 = [r|
      select
        record.id,
        uuid,
        di.title
      from record

      left join data_identification di on di.record_id = record.id 
      left join md_commons c on c.record_id = record.id 
      -- left join data_parameter on data_parameter.concept_id = concept_view.concept_id
      -- left join record on data_parameter.record_id = record.id
  |]
  xs :: [ 
    (Integer, 
    String, 
    String
    ) ] <- PG.query conn query1 ()
  mapM (putStrLn.show) xs
  return xs



main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  getRecordList conn
  return ()


