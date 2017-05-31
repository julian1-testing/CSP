{-
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability


  http://localhost:3000/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}

module Metadata where

import qualified Database.PostgreSQL.Simple as PG(connectPostgreSQL)

import qualified Data.ByteString.Char8 as BS(ByteString(..), append, empty, unpack )
import qualified Data.Text.Lazy as LT(pack, empty, append, fromStrict)
import qualified Data.Text.Lazy.IO as LT(putStrLn)
import qualified Data.Text.Encoding as E(decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LE(decodeUtf8, encodeUtf8)

import qualified Text.XML.HXT.DOM.Util as X(attrEscapeXml, textEscapeXml, stringEscapeXml)

import Text.RawString.QQ

import Record
import qualified RecordGet as RecordGet(getRecords, getRecordIdFromUuid)
import qualified Helpers as H(concatLT, pad)
import qualified Config as Config(connString)



-- TODO better way?
bsToLazy b =  LT.fromStrict $ E.decodeUtf8 b



formatXML records depth =
  H.concatLT $ map (\record -> formatRecord record depth) records
  -- H.concatLT $ map (flip $ formatRecord depth ) records
  where

    formatRecord record depth =
      H.concatLT [
        "\n", H.pad $ depth * 3, "<metadata>"
        ,

        -- TODO use maybe() - and flip the depth argument...
        case dataIdentification record of
          Just di -> formatTitle di (depth + 1)
        ,
        -- source (not uuid!!), probably not be needed
        "<source>ed23e365-c459-4aa4-bbc1-5d2cd0274af0</source>"
        ,

        -- image
        formatImage (depth + 1), "\n",

        -- Point of Truth
        H.pad $ depth * 3,
        "<link>|Point of truth URL of this metadata record|https://catalogue-imos.aodn.org.au:443/geonetwork/srv/en/metadata.show?uuid=",
          -- maybe  "" id (Just "hi")
          maybe "" LT.pack (uuid record),
          -- uuid record
          -- "aaad092c-c3af-42e6-87e0-bdaef945f522"
          "|WWW:LINK-1.0-http--metadata-URL|text/html</link>\n",

        -- responsibleParty doesn't do anything on step1,
        -- "<responsibleParty>resourceProvider|resource|Bureau of Meteorology (BOM)|</responsibleParty><responsibleParty>principalInvestigator|resource|Bureau of Meteorology (BOM)|</responsibleParty><responsibleParty>distributor|metadata|Integrated Marine Observing System (IMOS)|</responsibleParty>\n",

        -- parameter works - straight from vocab,
        -- "<parameter>Skin temperature of the water body</parameter>\n",

        -- organisation works
        "<organisation>Integrated Marine Observing System (IMOS)</organisation>\n",


        -- temp extent works
        "<tempExtentBegin>1992-03-19t14:00:00.000z</tempExtentBegin>\n",
        "<tempExtentEnd>2017-05-27t13:59:59.000z</tempExtentEnd>\n",


        -- is this an efficient way of doing this????
        -- foldl (\a b -> LT.append a $ LE.decodeUtf8  b) LT.empty  $ geopoly record,
        -- LT.fromStrict $ E.decodeUtf8 $ foldl (BS.append ) BS.empty $ polys

{-
        let polys = map (formatGeopoly $ depth + 1) $ geopoly record in
        foldl (LT.append ) LT.empty polys
-}
        formatGeopoly $ depth + 1
        ,

        -- dataparameters - eg. parameter, platform, organisation
        let dps = map (formatDataParameter $ depth + 1) $ dataParameters record in
        foldl (LT.append ) LT.empty dps
        ,

        let links = map (formatLink $ depth + 1) $ transferLinks record in
        foldl (LT.append ) LT.empty links
        ,

        -- geonet
        -- nothing appears to be used except the record uuid 
        [r|
          <geonet:info xmlns:geonet="http://www.fao.org/geonetwork" >
              <id>153</id>
        |],
              "<uuid>",  maybe ( "") LT.pack ( uuid record) , "</uuid>",
        [r|
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

        H.pad $ depth * 3, "</metadata>"
      ]


    formatTitle di depth  =
      H.concatLT [
          "\n",
          H.pad $ depth * 3,
          "<title>", LT.pack $ title di, LT.pack "</title>"
      ]


    formatGeopoly depth =
      -- <geoPolygon>POLYGON ((-85 -15, -85 -10, -80 -10, -80 -15, -85 -15))</geoPolygon>

      -- metadata <gml:posList srsDimension="2">-75 -10 -75 -15 -70 -15 -70 -45 -75 -45 -75 -50 -70 -50 -70 -55 -65 -55 -65 -45 -60 -45 -60 -35 -55 -35 -55 -30 -50 -30 -50 -25 -45 -25 -45 -20 -40 -20 -40 -5 -50 -5 -50 0 -55 0 -55 5 -65 5 -65 10 -75 10 -75 -10</gml:posList>
      -- note the ,
      H.concatLT [
          "\n",
          H.pad $ depth * 3,
          -- "<geoPolygon> ", bsToLazy geopoly, "</geoPolygon>"
          -- "<geoPolygon>POLYGON ((-85 -15, -85 -10, -80 -10, -80 -15, -85 -15))</geoPolygon>"
          [r|
<geoPolygon>POLYGON ((180 -70, 180 -75, 170 -75, 170 -80, 160 -80, 160 -70, 135 -70, 135 -65, 130 -65, 130 -70, 100 -70, 100 -65, 95 -65, 95 -70, 0 -70, 0 -75, -40 -75, -40 -70, -45 -70, -45 -75, -50 -75, -50 -70, -55 -70, -55 -65, -65 -65, -65 -70, -80 -70, -80 -75, -110 -75, -110 -70, -115 -70, -115 -75, -145 -75, -145 -80, -180 -80, -180 65, -175 65, -175 60, -165 60, -165 55, -160 55, -160 60, -150 60, -150 65, -145 65, -145 60, -130 60, -130 55, -125 55, -125 50, -120 50, -120 35, -115 35, -115 30, -110 30, -110 25, -105 25, -105 20, -100 20, -100 30, -90 30, -90 40, -85 40, -85 35, -80 35, -80 40, -75 40, -75 45, -55 45, -55 50, -60 50, -60 55, -65 55, -65 65, -60 65, -60 75, -55 75, -55 70, -50 70, -50 65, -40 65, -40 70, -20 70, -20 75, -15 75, -15 85, -25 85, -25 90, 70 90, 70 85, 40 85, 40 80, 30 80, 30 75, 50 75, 50 70, 20 70, 20 65, 25 65, 25 50, 20 50, 20 55, 15 55, 15 65, 10 65, 10 50, 15 50, 15 45, 30 45, 30 50, 35 50, 35 45, 45 45, 45 40, 40 40, 40 20, 55 20, 55 30, 65 30, 65 25, 70 25, 70 20, 75 20, 75 15, 80 15, 80 25, 95 25, 95 20, 100 20, 100 0, 105 0, 105 20, 110 20, 110 25, 120 25, 120 30, 125 30, 125 45, 135 45, 135 50, 145 50, 145 55, 150 55, 150 50, 155 50, 155 55, 160 55, 160 60, 170 60, 170 65, 180 65, 180 -70), (-75 -10, -75 -15, -70 -15, -70 -45, -75 -45, -75 -50, -70 -50, -70 -55, -65 -55, -65 -45, -60 -45, -60 -35, -55 -35, -55 -30, -50 -30, -50 -25, -45 -25, -45 -20, -40 -20, -40 -5, -50 -5, -50 0, -55 0, -55 5, -65 5, -65 10, -75 10, -75 -10), (15 -15, 15 -30, 30 -30, 30 -25, 35 -25, 35 -15, 40 -15, 40 -10, 35 -10, 35 0, 40 0, 40 5, 45 5, 45 10, 40 10, 40 15, 35 15, 35 25, 25 25, 25 30, 10 30, 10 35, -5 35, -5 30, -10 30, -10 25, -15 25, -15 10, 5 10, 5 5, 10 5, 10 -5, 15 -5, 15 -15), (120 -10, 135 -10, 135 -15, 125 -15, 125 -20, 115 -20, 115 -30, 135 -30, 135 -35, 150 -35, 150 -20, 145 -20, 145 -15, 140 -15, 140 -5, 120 -5, 120 -10), (165 -65, 165 -70, 170 -70, 170 -65, 165 -65), (110 -5, 115 -5, 115 0, 110 0, 110 -5), (5 45, 5 60, -5 60, -5 50, 0 50, 0 45, 5 45), (20 80, 20 75, 25 75, 25 80, 20 80))</geoPolygon>
          |]
      ]


    formatDataParameter depth dp =
      H.concatLT [
        "\n",
        H.pad $ depth * 3,
        -- f (label, url, rootLabel) =
        case dp of

          DataParameter label _ "AODN Parameter Category Vocabulary" ->
            H.concatLT [ "<parameter>", bsToLazy label, "</parameter>" ]

          DataParameter label _ "AODN Platform Category Vocabulary" ->
            H.concatLT [ "<platform>", bsToLazy label, "</platform>" ]

          DataParameter label _ "AODN Organisation Category Vocabulary" ->
            H.concatLT [ "<organisation>", bsToLazy label, "</organisation>" ]

          _ -> ""
      ]


    {-
        data TransferLink = TransferLink {

            protocol :: BS.ByteString,
            linkage :: BS.ByteString,
            description :: BS.ByteString
        } deriving (Show, Eq)

        <link>imos:anmn_velocity_timeseries_map|Moorings - velocity time-series|http://geoserver-123.aodn.org.au/geoserver/wms|OGC:WMS-1.1.1-http-get-map|application/vnd.ogc.wms_xml</link>
    -}

    formatLink depth link =
      H.concatLT [
        "\n",
        H.pad $ depth * 3,
        case link of
          -- TransferLink protocol linkage name description | protocol == "OGC:WMS-1.1.1-http-get-map" -> H.concatLT [
          TransferLink protocol linkage name description -> H.concatLT [
              "<link>",
                bsToLazy name,
                "|", LT.pack $ X.textEscapeXml $ BS.unpack description,

                -- need to xmlescap the & in links
                "|", LT.pack $ X.textEscapeXml $ BS.unpack linkage,

                "|", bsToLazy protocol,
                -- GN appends a mime type as well supposedly discovered dynamically...
                -- "|application/vnd.ogc.wms_xml",
              "</link>"
            ]
          -- _ -> ""
          -- TransferLink protocol linkage name description -> H.concatLT [ "<!-- ", bsToLazy protocol, bsToLazy linkage, bsToLazy name, bsToLazy description, "-->" ]
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


----
-- tests




main :: IO ()
main = do
  -- TODO currently assumes argo is populated in the db.
  conn <- PG.connectPostgreSQL Config.connString

  --- records <- RecordGet.getRecords conn [ 289, 290 ]
  argo_id <- RecordGet.getRecordIdFromUuid conn "4402cb50-e20a-44ee-93e6-4728259250d2" -- argo

  print argo_id

  case argo_id of
    Just record_id ->  do
      -- records <- RecordGet.getRecords conn [ 289, 290 ]
      records <- RecordGet.getRecords conn [ record_id ]

      mapM (putStrLn.show) records

      let s = formatXML records 0
      LT.putStrLn $ s

    Nothing -> do
      putStrLn "couldn't find argo"




