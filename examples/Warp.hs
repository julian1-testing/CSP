{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app


-- how would we throttle this according to the number of db connections - until one was available?
-- 

app :: Application
app req f = do
    print "got request"
    x <- f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!" 
    print "done request"
    return x

