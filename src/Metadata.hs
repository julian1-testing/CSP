{-
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability


  http://localhost:3000/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}

module Metadata where

import qualified Database.PostgreSQL.Simple as PG(connectPostgreSQL)

import qualified Data.ByteString.Char8 as BS(ByteString(..), append, empty, unpack, split, concat)
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


        let polys = map (formatGeopoly $ depth + 1) $ geopoly record in
        foldl (LT.append ) LT.empty polys

        -- formatGeopoly $ depth + 1
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


    formatGeopoly depth poly =
      -- TODO treat geometry as real geometry in the db
      -- turn -180 -90 -180 -85 -175 -85 -175 -90 -180 -90  
      -- into -180 -90, -180 -85, -175 -85, -175 -90, -180 -90 
      let vals = BS.split ' ' poly in
      let enumeratedVals = zip [0..] vals in 
      let f s (index,val) = 
            case index `mod` 2 of 
              1 | index /= length vals - 1 -> BS.concat [ s, " ", val, "," ]
              1 -> BS.concat [ s, " ", val ]
              0 -> BS.concat [ s, " ", val ]
      in
      let s = foldl f "" enumeratedVals in

      H.concatLT [
          "\n",
          H.pad $ depth * 3,
          "<geoPolygon>POLYGON ((", bsToLazy s, "))</geoPolygon>"
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




