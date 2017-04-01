
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}


import Text.XML.HXT.Core


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC


import Database.PostgreSQL.Simple
import Text.RawString.QQ

main :: IO ()
main = do
    print "hithere"

    conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

{-
    let query = [r|
        select id, label 
        from concept_view  
        where parent_id is null 
        and scheme_title = 'AODN Platform Category Vocabulary'
        and scheme_title = ?
    |]
-}
    -- xs :: [ (Integer, String) ] <- query conn query (Only "AODN Platform Category Vocabulary")

    let url = "whoot"  :: String

    xs :: [ (Integer, String) ] <- query conn "select id, label from concept where url = ?" (Only url)
 

    return ()




