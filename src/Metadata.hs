{-
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability


  http://localhost:3000/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}

module Metadata where

import qualified Database.PostgreSQL.Simple as PG(connectPostgreSQL)

import qualified Data.ByteString.Char8 as BS(ByteString(..), append, empty )

import qualified Data.Text.Lazy as LT(pack, empty, append, fromStrict)
import qualified Data.Text.Lazy.IO as LT(putStrLn)

import qualified Data.Text.Encoding as E(decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LE(decodeUtf8, encodeUtf8)

import Text.RawString.QQ

import Record
import qualified RecordGet as RecordGet(getRecords)
import qualified Helpers as H(concatLT, pad)



formatXML records depth =
  H.concatLT $ map (\record -> formatRecord record depth) records
  -- H.concatLT $ map (flip $ formatRecord depth ) records
  where

    formatRecord record depth =
      H.concatLT [
        "\n", H.pad $ depth * 3, "<metadata>"
        ,
        case uuid record of
          Just uuid_ -> formatSource uuid_ (depth + 1)
        ,
        case dataIdentification record of
          Just di -> formatTitle di (depth + 1)
        ,

        formatImage (depth + 1), "\n",

        -- POT works -> fills in the more
        H.pad $ depth * 3,
        "<link>|Point of truth URL of this metadata record|https://catalogue-imos.aodn.org.au:443/geonetwork/srv/en/metadata.show?uuid=aaad092c-c3af-42e6-87e0-bdaef945f522|WWW:LINK-1.0-http--metadata-URL|text/html</link>\n",

        -- does not appear do anything,
        -- "<responsibleParty>resourceProvider|resource|Bureau of Meteorology (BOM)|</responsibleParty><responsibleParty>principalInvestigator|resource|Bureau of Meteorology (BOM)|</responsibleParty><responsibleParty>distributor|metadata|Integrated Marine Observing System (IMOS)|</responsibleParty>\n",

        -- parameter works - straight from vocab,
        "<parameter>Skin temperature of the water body</parameter>\n",

        -- organisation works
        "<organisation>Integrated Marine Observing System (IMOS)</organisation>\n",

        -- platform works
        "<platform>NOAA-17</platform>\n",

        -- temp extent works,
        "<tempExtentBegin>1992-03-19t14:00:00.000z</tempExtentBegin>\n",
        "<tempExtentEnd>2017-05-27t13:59:59.000z</tempExtentEnd>\n",

        -- it looks like the geobox works -- but not
        -- this should be really easy to do - because already in the db...
        "<geoBox>170|-70|70|20</geoBox>\n",

        -- has to be a fold and append 
        -- concatLT lst = foldl LT.append LT.empty lst

        -- LE(encodeUtf8)

        -- is this an efficient way of doing this????
         -- foldl (\a b -> LT.append a $ LE.decodeUtf8  b) LT.empty  $ geopoly record,

        -- rather than doing 
        -- we should do a map on the entries first 
        let polys = map (\a -> formatGeopoly a  1) $ geopoly record in
        -- LT.fromStrict $ E.decodeUtf8 $ foldl (BS.append ) BS.empty $ polys
        foldl (LT.append ) LT.empty $ polys
        ,



        -- nothing in the geonet appears to work
{-
        [r|
          <geonet:info xmlns:geonet="http://www.fao.org/geonetwork" >
              <id>153</id>
              <uuid>aaad092c-c3af-42e6-87e0-bdaef945f522</uuid>
              <schema>iso19139.mcp-2.0</schema>
              <createDate>2016-05-25T16:35:13</createDate>
              <changeDate>2017-05-27T00:00:03</changeDate>
              <source>ed23e365-c459-4aa4-bbc1-5d2cd0274af0</source>
              <view>true</view>
              <notify>false</notify>
              <download>false</download>
              <dynamic>true</dynamic>
              <featured>true</featured>
              <guestdownload>false</guestdownload>
              <selected>false</selected>
            </geonet:info>
        |],
-}
        H.pad $ depth * 3, "</metadata>"
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

    formatGeopoly geopoly depth =
      H.concatLT [
          "\n",
          -- H.pad $ depth * 3,
          "<geoPolygon>", LT.fromStrict $ E.decodeUtf8 geopoly, "</geoPolygon>"
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


  mapM (putStrLn.show) records

  let s = formatXML records 0

  LT.putStrLn $ s



