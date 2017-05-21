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
import qualified RecordGet as RecordGet(getRecords)
import qualified Helpers as H(concatLT, pad)



formatXML records depth =
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

        formatImage (depth + 1),

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

    formatImage depth =
      -- appears that portal disregards the image link in favor of explicit lookup... 
      H.concatLT [
          "\n",
          H.pad $ depth * 3,
          -- "<image>thumbnail|http://whoot/image.jpg</image>"
          -- <image>thumbnail|../../srv/en/resources.get?uuid=c317b0fe-02e8-4ff9-96c9-563fd58e82ac&fname=gliders_map_s.png&access=public</image>
          "<image>https://portal.aodn.org.au/images/AODN/AODN_logo_fullText.png</image>"
      ]





main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  records <- RecordGet.getRecords conn [ 289, 290 ]

  let s = formatXML records 0

  LT.putStrLn $ s



