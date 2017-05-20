{-
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability


  http://localhost:3000/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

{-# LANGUAGE OverloadedStrings #-}

module Metadata where


import qualified Database.PostgreSQL.Simple as PG(connectPostgreSQL)
import qualified Data.Text.Lazy as LT(pack)
import qualified Data.Text.Lazy.IO as LT(putStrLn)


import Record
import RecordGet as RecordGet(getRecords)
import qualified Helpers as H(concatLT, pad)



formatRecords records depth =  
  H.concatLT $ map (\record -> formatRecord record depth) records
  -- H.concatLT $ map (flip $ formatRecord depth ) records
  where

    formatRecord record depth =  
      H.concatLT [
        "\n",
        H.pad $ depth * 3,
        "<metadata>"
        ,
        case uuid record of 
          Just uuid_ -> formatSource uuid_ (depth + 1)
        ,
        case dataIdentification record of
          Just di -> formatTitle di (depth + 1)
        ,
        "\n",
        H.pad $ depth * 3,
        "</metadata>"
      ]

    formatTitle di depth  = 
      H.concatLT [
          "\n",
          H.pad $ depth * 3,
          "<title>", LT.pack $ title di, LT.pack "</title>"
      ]

    formatSource uuid depth =   
      H.concatLT [
          "\n",
          H.pad $ depth * 3,
          "<source>", LT.pack uuid, "</source>"
      ]




main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  {- 
    what we want is a list of record_id's  - which we have, in the propgated root node- 
      1. then can do a query to join - to get a list  
      2. then xml format

    think we also want the pad...

    - ok, lets try to return a compounded list structure...
      it's going to be a different call...
  -}

  {-
  record <- RecordGet.getRecord conn 289

  (putStrLn.show.fromJust.uuid) $ record
  (putStrLn.show.title.fromJust.dataIdentification ) $ record
  (putStrLn.show) $ record

  let depth = 0
  -}

  records <- RecordGet.getRecords conn [ 289, 290 ]
  
  let s = formatRecords records 0

  LT.putStrLn $ s



