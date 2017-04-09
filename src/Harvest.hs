{-
  Use CSW and RecordStore modules - to harvest records from CSW server to local db

-}

{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}

module Harvest where

import Text.XML.HXT.Core(runX, (>>>))
import Database.PostgreSQL.Simple as PG(connectPostgreSQL, close)
import Text.RawString.QQ

import qualified Helpers as Helpers
import qualified CSW as CSW
-- import qualified Record --as Record(Record(..))--(Record, dataIdentification, transferLinks, showRecord, Record)
import qualified Record as R
import qualified RecordStore as RS
import qualified ParseMCP20 as ParseMCP20


-- apply left or right function according to Either type
-- probably in stdlib
applyEither lf rf x 
  = case x of
    Right a -> lf a
    Left a -> rf a


----------------

doGetAndProcessRecord conn uuid title = do

    print $ "doGetAndProcessRecord " ++ uuid ++ " " ++ title

    recordText <- CSW.doGetRecordById uuid title
    myRecord <- ParseMCP20.parse $ Helpers.parseXML recordText
    -- putStrLn $ applyEither Record.showRecord id myRecord 

    case myRecord of
        Right record -> do
            RS.storeAll conn record
            return ()
        Left msg -> do
          -- print $ concatMap id [ "error ", msg, " ",  uuid, title ]
          print $ mconcat [ "error ", msg, " ",  uuid, title ]

    return ()




doGetAndProcessRecords conn = do

    -- delete current index
    RS.deleteAll conn
    
    -- get records to process
    result <- CSW.doGetRecords "https://catalogue-imos.aodn.org.au/geonetwork" 
    let elts = Helpers.parseXML result
    identifiers <- runX (elts >>> CSW.parseCSWSummaryRecord)
    mapM (putStrLn.show) identifiers
 
    -- process each record
    mapM (uncurry $ doGetAndProcessRecord conn) identifiers



main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  doGetAndProcessRecords conn 
  PG.close conn

  return ()


    -- TODO if showRecord was just show, then we wouldn't have to destructure
    {-
    putStrLn $ case myRecord of
        Right record -> 
          R.showRecord record
        Left msg -> 
          msg
    -}

  -- change name store to store
  -- uuid should match....
  -- 

    -- myRecord <- ParseMCP20.parse elts

