-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

{-# LANGUAGE OverloadedStrings #-}


import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)



main :: IO ()
main = do
    -- let timeout = responseTimeoutMicro 100

    let settings = tlsManagerSettings  { managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000 } 
    manager <- newManager settings 


    let url = "https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=http://www.isotc211.org/2005/gmd" 
    request <- parseRequest url -- "http://httpbin.org/post"

    response <- httpLbs request manager


    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
      -- putStrLn $ "The status code was: " ++ (show $ RESponseStatus response)

    -- s <- responseBody response

    print $ responseBody response



