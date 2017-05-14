{-
  
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

{-# LANGUAGE OverloadedStrings #-}


module Metadata where


import Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL, Only(..))

import RecordGet as RecordGet(getRecord)

-- So we actually need to format the damn xml...


main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  let record_id = 289

  record <- RecordGet.getRecord conn record_id 

  (putStrLn.show) record 


