-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Text.XML.HXT.Core

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

import Data.ByteString.Lazy.Char8(unpack)



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

getIdentifiers = do

    let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%*%&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000"
    print "done"


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
    let s = unpack $ responseBody response
    onlineResources <- runX (parseXML s  >>> parseOnlineResources)
    let lst = Prelude.map (\(a,b) -> " ->" ++ a ++ " ->" ++ b ) onlineResources
    mapM print lst
    print "finished"


main :: IO ()
main = getResources

