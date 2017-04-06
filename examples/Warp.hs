{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

{-
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
-}

import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Encoding as LE



main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app


-- how would we throttle this according to the number of db connections - until one was available?
-- 

app :: Application
app req f = do
    print "got request"

    -- this works by itself, it's using Data.Text.Text
    let a = LT.pack "čušpajž日本語"  
    let b = LT.pack " whoot"  
    let c = LT.append a b

    -- 
    let d = LE.encodeUtf8 c


    -- x <- f $ responseLBS status200 [(hContentType, "text/plain")] d --"Hello world!" 
    -- x <- f $ responseLBS status200 [(hContentType, "text/plain")] d --"Hello world!" 
    x <- f $ responseLBS status200 [(hContentType, "text/html; charset=utf-8")] d --"Hello world!" 

    print "done request"
    return x

