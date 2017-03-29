-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Text.XML.HXT.Core

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

-- import Network.HTTP.Types.Method.Method
import Network.HTTP.Types.Method

import Network.HTTP.Types.Header

-- import Data.ByteString.Lazy.Char8(unpack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC




parseXML s = readString [ withValidate no
    , withRemoveWS yes  -- throw away formating WS
    ] s


atTag tag = deep (isElem >>> hasName tag)


-- limit to just the wms/wfs stuff.
--
parseOnlineResources = atTag "gmd:CI_OnlineResource" >>>
  proc l -> do
    -- leagName <- getAttrValue "NAME"   -< l
    protocol <- atTag "gmd:protocol" >>> getChildren >>> hasName "gco:CharacterString" >>> getChildren >>> getText -< l
    url      <- atTag "gmd:linkage"  >>> getChildren >>> hasName "gmd:URL" >>> getChildren >>> getText -< l
    returnA -< (protocol, url)




-- ok, we want to hit the main metadata....
-- one function to extract and one to download - and parametize the actual url.

-- change name to doGetRecords

-- this is hell.... we're going to have to escape everything ....



doGetRecords1 = do
    -- let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%*%&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000"
    -- OK, the following url is a valid url
    -- let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%25argo%25&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000"

    -- either the url encoding complains about the delimiter, - invalidURL
    -- or the cql filter complains,
    -- let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=\"csw:AnyText+Like+'%*%'\"&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000"
    -- let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=\"csw:AnyText+Like+\'%*%\'\"&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000"
    -- let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=csw:AnyText+Like+'%*%'&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000"
    let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+Like+%27%25argo%25%27&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000"
    -- ' is %27
    -- % is %25
    response <- doHTTP url
    let s = BLC.unpack $ responseBody response
    print s




-- QuasiQuotes may be cleaner,
-- http://kseo.github.io/posts/2014-02-06-multi-line-strings-in-haskell.html
query :: String
query = unlines [
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<csw:GetRecords xmlns:csw=\"http://www.opengis.net/cat/csw/2.0.2\" service=\"CSW\" version=\"2.0.2\"    ",
    "    resultType=\"results\" startPosition=\"1\" maxRecords=\"5\" outputFormat=\"application/xml\"  >",
    "  <csw:Query typeNames=\"csw:Record\">",
    "    <csw:Constraint version=\"1.1.0\">",
    "      <Filter xmlns=\"http://www.opengis.net/ogc\" xmlns:gml=\"http://www.opengis.net/gml\">",
    "        <PropertyIsLike wildCard=\"%\" singleChar=\"_\" escape=\"\\\">",
    "          <PropertyName>AnyText</PropertyName>",
    "          <Literal>%</Literal>",
    "        </PropertyIsLike>",
    "      </Filter>",
    "    </csw:Constraint>",
    "  </csw:Query>",
    "</csw:GetRecords>" ]


-- curl -k -v -H "Content-Type: application/xml"   -X POST -d @query.xml 'https://catalogue-123.aodn.org.au/geonetwork/srv/eng/csw' 2>&1  | less



doPost url body = do
    let settings = tlsManagerSettings  {
        managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000
    }
    manager <- newManager settings
    -- get initial request
    initialRequest <- parseRequest url
    -- modify for post
    let request = initialRequest {
        method = BC.pack "POST",
        requestBody = RequestBodyBS $ BC.pack body,
        requestHeaders = [
            (hContentType, BC.pack "application/xml")
        ]
    }
    response <- httpLbs request manager
    Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    return response



doGetRecords = do
    let url = "https://catalogue-123.aodn.org.au/geonetwork/srv/eng/csw"
    let body = query
    response <- doPost url body
    let s = BLC.unpack $ responseBody response
    print s



-- manager <- newManager settings



doHTTP url = do
    let settings = tlsManagerSettings  { managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000 }
    manager <- newManager settings
    request <- parseRequest url
    response <- httpLbs request manager
    Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    return response



getResources = do
    let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=http://www.isotc211.org/2005/gmd"
    response <- doHTTP url
    let s = BLC.unpack $ responseBody response
    onlineResources <- runX (parseXML s  >>> parseOnlineResources)
    let lst = Prelude.map (\(a,b) -> " ->" ++ a ++ " ->" ++ b ) onlineResources
    mapM print lst
    print "finished"


main :: IO ()
-- main = getResources
main = doGetRecords
-- main = do
--    putStrLn query

