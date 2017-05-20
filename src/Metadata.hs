{-

  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability


  http://localhost:3000/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

{-# LANGUAGE OverloadedStrings #-}


module Metadata where


import Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL, Only(..))

import RecordGet as RecordGet(getRecord)
import Record


import qualified Data.Text.Lazy as LT(pack, empty, append)
import qualified Data.Text.Lazy.IO as LT(putStrLn)

import Data.Maybe


import qualified Helpers as H(concatLT, pad)


-- TODO use Option.maybe()
maybeToString m = 
  LT.pack $ case m of
    Just uuid -> uuid
    Nothing -> ""

-- OK. so if we have a set of record_id - we should be able to format them all 




main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  {- 
    what we want is a list of record_id's  - which we have, in the propgated root node- 
      1. then can do a query to join - to get a list  
      2. then xml format

    think we also want the pad...
  -}
  let record_id = 289

  record <- RecordGet.getRecord conn record_id

  (putStrLn.show.fromJust.uuid) $ record
  (putStrLn.show.title.fromJust.dataIdentification ) $ record
  (putStrLn.show) $ record

  let depth = 0

  let s = H.concatLT [
          "<metadata>",


          case uuid record of 
            Just uuid_ -> formatSource uuid_ (depth + 1)
          ,

          case dataIdentification record of
            Just di -> formatTitle di (depth + 1)
          ,

          "\n</metadata>"
        ]
        where
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





  LT.putStrLn $ s



