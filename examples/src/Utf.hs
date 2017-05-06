
-- http://stackoverflow.com/questions/2086842/using-haskell-to-output-a-utf-8-encoded-bytestring

-- {-# LANGUAGE OverloadedStrings #-}

module Utf where

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import qualified Data.ByteString as B

-- https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString.html
-- cons  to append a single character

main =  do

  -- this works by itself, it's using Data.Text.Text
  let a = T.pack "čušpajž日本語"  
  let b = T.pack " whoot日本語"  


  -- let y = B.append a a 
  -- T.putStrLn $ "čušpajž日本語" ++ y

  T.putStrLn $ T.append a  b


