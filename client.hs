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



getOnlineResources1 = atTag "gmd:CI_OnlineResource" >>>
  proc l -> do
    -- leagName <- getAttrValue "NAME"   -< l
    protocol <- atTag "gmd:protocol" >>> getChildren >>> hasName "gco:CharacterString" >>> getChildren >>> getText -< l
    url      <- atTag "gmd:linkage"  >>> getChildren >>> hasName "gmd:URL" >>> getChildren >>> getText -< l
    returnA -< (protocol, url)



main :: IO ()
main = do
    let settings = tlsManagerSettings  { managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000 }
    manager <- newManager settings


    let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=http://www.isotc211.org/2005/gmd"
    request <- parseRequest url

    response <- httpLbs request manager

    Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)

    -- convert internal.ByteString to String
    let s = unpack $ responseBody response

    teams <- runX (parseXML s  >>> getOnlineResources1)
    let lst = Prelude.map (\(a,b) -> " ->" ++ a ++ " ->" ++ b ) teams
    mapM print lst


    print "done"

