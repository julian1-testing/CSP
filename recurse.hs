
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}

-- import Text.XML.HXT.Core

{-
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
-}

import Database.PostgreSQL.Simple

import Text.RawString.QQ


main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"


  let query1 = [r|
        select id, label 
        from concept_view  
        where parent_id is null 
        and scheme_title ~ ?
  |]

  let url = "Platform"  :: String

  xs :: [ (Integer, String) ] <- query conn query1 (Only url)

  mapM print xs

  return ()




