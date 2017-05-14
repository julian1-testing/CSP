{-
  
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}


module Metadata where


import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Text.RawString.QQ




getRecordList conn = do

  let query1 = [r|
      select
        concept_view.concept_id,
        concept_view.parent_id,
        data_parameter.record_id
      from concept_view
      left join data_parameter on data_parameter.concept_id = concept_view.concept_id
      left join record on data_parameter.record_id = record.id
  |]
  xs :: [ 
    (Integer, 
    Maybe Integer, 
    Maybe Integer 
    ) ] <- PG.query conn query1 ()
  mapM (putStrLn.show) xs
  return xs


-- main = getRecordList 


main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  getRecordList conn

  -- s <- request conn
  --- LT.putStrLn $ s

  return ()


