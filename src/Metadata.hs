{-
  Format the metadata part of xml.search.imos

  https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability


  http://localhost:3000/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}


module Metadata where

import qualified Database.PostgreSQL.Simple as PG(connectPostgreSQL)

import qualified Data.ByteString.Char8 as BS(ByteString(..), unpack, split, concat)
import qualified Data.ByteString.Lazy.Char8 as LBS(readFile, fromChunks) 
import qualified Data.Text.Lazy as LT(pack, empty, append, fromStrict, concat)
import qualified Data.Text.Lazy.IO as LT(putStrLn)
import qualified Data.Text.Encoding as E(decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LE(decodeUtf8, encodeUtf8)

import qualified Text.XML.HXT.DOM.Util as X(attrEscapeXml, textEscapeXml, stringEscapeXml)

import Text.RawString.QQ

import Record
import qualified RecordGet as RecordGet(getRecords, getRecordIdFromUuid)
import qualified Helpers as H(pad)
import qualified Config as Config(connString)



-- TODO better way?
-- fromChunks ?
bsToLazy s =  LT.fromStrict $ E.decodeUtf8 s

bsToLazyEscaped s = LT.pack $ X.textEscapeXml $ BS.unpack s



formatXML records depth =

  LT.concat $ map (formatRecord depth) records
  where
    formatRecord depth record =

      let nextDepth = depth + 1 in
      LT.concat [

        "\n", 
        H.pad $ depth * 3, "<metadata>",

        maybe "" (formatDataIdentification nextDepth) $ dataIdentification record,

        -- this just returns the uuid as the source - so we could adjust based on that.
        "\n",
        "<source>1</source>"
        ,

        -- image
        formatImage nextDepth, 

        formatPOT nextDepth $ uuid record,

        -- responsibleParty doesn't do anything on step1,
        -- "<responsibleParty>resourceProvider|resource|Bureau of Meteorology (BOM)|</responsibleParty><responsibleParty>principalInvestigator|resource|Bureau of Meteorology (BOM)|</responsibleParty>
        -- <responsibleParty>distributor|metadata|Integrated Marine Observing System (IMOS)|</responsibleParty>\n",

        -- organisation works, but keyword only
        "<organisation>Integrated Marine Observing System (IMOS)</organisation>\n",

        -- temp extent works
        -- we have a real temp-extent.
        "<tempExtentBegin>1992-03-19t14:00:00.000z</tempExtentBegin>\n",
        "<tempExtentEnd>2017-05-27t13:59:59.000z</tempExtentEnd>\n",


        LT.concat $ map (formatDataParameter nextDepth) $ dataParameters record,
  
        LT.concat $ map (formatLink nextDepth) $ transferLinks record,

        LT.concat $ map (formatUseLimitation nextDepth) $ useLimitations record,

        LT.concat $ map (formatAttrConstraint nextDepth) $ attrConstraints record,

        maybe "" (formatMDCommons nextDepth) $ mdCommons record, 



        LT.concat $ map (formatGeopoly nextDepth) $ geopoly record,



        -- geonet
        -- looks like nothing here is used, except the record uuid
        [r|
          <geonet:info xmlns:geonet="http://www.fao.org/geonetwork" >
              <id>153</id>
        |],
              "<uuid>",  maybe "" bsToLazy $ uuid record, "</uuid>",
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



    formatPOT depth uuid =
        -- TODO pass the source catalogue as well....
        -- Point of Truth
      LT.concat [
          H.pad $ depth * 3,
          "<link>|Point of truth URL of this metadata record|https://catalogue-imos.aodn.org.au:443/geonetwork/srv/en/metadata.show?uuid=",
          maybe "" bsToLazy uuid,
          "|WWW:LINK-1.0-http--metadata-URL|text/html",
          "</link>\n"
      ]


    formatDataIdentification depth di =
      LT.concat [
          "\n", H.pad $ depth * 3,
          "<title>", bsToLazy $ title di, "</title>"
      ]


{-
    <imageLink>https://licensebuttons.net/l/by/4.0/88x31.png</imageLink>
    <jurisdictionLink>http://creativecommons.org/international/</jurisdictionLink>
    <licenseName>Attribution 4.0 International</licenseName>
    <licenseLink>http://creativecommons.org/licenses/by/4.0/</licenseLink>
-} 

    formatMDCommons depth md =
      let pad = LT.concat [ "\n", H.pad $ depth * 3 ] in
      LT.concat [
          pad, "<imageLink>", bsToLazy $ licenseImageLink md, "</imageLink>",
          pad, "<jurisdictionLink>", bsToLazy $ jurisdictionLink md, "</jurisdictionLink>",
          pad, "<licenseName>", bsToLazy $ licenseName md, "</licenseName>",
          pad, "<licenseLink>", bsToLazy $ licenseLink md, "</licenseLink>"
      ]


    formatSource depth source =
      let source' = LT.pack $ X.textEscapeXml source in
      LT.concat [
          "\n", H.pad $ depth * 3,
          "<source>", source', "</source>"
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

      LT.concat [
          "\n",
          H.pad $ depth * 3,
          "<geoPolygon>POLYGON ((", bsToLazy s, "))</geoPolygon>"
      ]


    formatDataParameter depth dp =
      LT.concat [
        "\n",
        H.pad $ depth * 3,
        -- f (label, url, rootLabel) =
        case dp of

          DataParameter label _ "AODN Parameter Category Vocabulary" ->
            LT.concat [ "<parameter>", bsToLazy label, "</parameter>" ]

          DataParameter label _ "AODN Platform Category Vocabulary" ->
            LT.concat [ "<platform>", bsToLazy label, "</platform>" ]

          DataParameter label _ "AODN Organisation Category Vocabulary" ->
            LT.concat [ "<organisation>", bsToLazy label, "</organisation>" ]

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
      LT.concat [
        "\n",
        H.pad $ depth * 3,
        case link of
          -- TransferLink protocol linkage name description | protocol == "OGC:WMS-1.1.1-http-get-map" -> LT.concat [
          TransferLink protocol linkage name description -> LT.concat [
              "<link>",
                bsToLazy name,
                "|", bsToLazyEscaped description,
                "|", bsToLazyEscaped linkage,
                "|", bsToLazy protocol,
                -- GN appends a mime type as well supposedly discovered dynamically...
                -- "|application/vnd.ogc.wms_xml",
              "</link>"
            ]
          -- _ -> ""
          -- TransferLink protocol linkage name description -> LT.concat [ "<!-- ", bsToLazy protocol, bsToLazy linkage, bsToLazy name, bsToLazy description, "-->" ]
      ]


    formatUseLimitation depth limitation = 
      LT.concat [
          "\n",
          H.pad $ depth * 3,
          "<useLimitation>", bsToLazyEscaped limitation, "</useLimitation>"
      ]


    formatAttrConstraint depth constraint = 
      LT.concat [
          "\n",
          H.pad $ depth * 3,
          "<attrConstr>", bsToLazyEscaped constraint, "</attrConstr>"
      ]


    formatImage depth =
      -- appears that portal disregards the image link in favor of explicit lookup...
      LT.concat [
          "\n",
          H.pad $ depth * 3,
          -- "<image>thumbnail|http://whoot/image.jpg</image>"
          -- <image>thumbnail|../../srv/en/resources.get?uuid=c317b0fe-02e8-4ff9-96c9-563fd58e82ac&fname=gliders_map_s.png&access=public</image>
          -- HOw does this work...
          "<image>https://portal.aodn.org.au/images/AODN/AODN_logo_fullText.png</image>",
          "\n"
      ]


----
-- tests

trComma ',' = '\n'
trComma x = x


main :: IO ()
main = do
  -- expect argo in the db
  conn <- PG.connectPostgreSQL Config.connString

  --- records <- RecordGet.getRecords conn [ 289, 290 ]
  argo_id <- RecordGet.getRecordIdFromUuid conn "4402cb50-e20a-44ee-93e6-4728259250d2" -- argo
  print argo_id

  maybe
      (putStrLn "not found")
      (\record_id -> do

          records <- RecordGet.getRecords conn [ record_id ]
          mapM (putStrLn. map trComma .show) records
          LT.putStrLn $ formatXML records 0
      )
      argo_id


