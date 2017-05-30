
{-# LANGUAGE OverloadedStrings #-}


module Config where

import qualified Data.ByteString.Char8 as BS(ByteString(..))


connString :: BS.ByteString
connString = "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

