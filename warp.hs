{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app


-- what's the damn monad here,

app :: Application
app req f = do
    print "got request"
    f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!" 

