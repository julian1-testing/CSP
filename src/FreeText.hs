
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes #-}

module FreeText where

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import Text.RawString.QQ


search conn query = do

    print "store uuid"


    xs :: [ (Only Int)] <- PG.query conn
        [r|

            select record_id from ( 
              select 
                record_id, 
                ( to_tsvector( title) || to_tsvector( abstract)) @@ to_tsquery( ?) as result 
                from data_identification
              ) as x 
            where x.result = true;
        |]

        $ Only (query :: String )
        -- (query :: Only String)

    -- got to be a map 

    {-
    let record_id = case xs of
         [] -> -99999 -- avoided because sql will return a value
         [ Only record_id ] -> record_id

    putStrLn $ "record_id is " ++ show record_id
    return record_id
    -}

    -- return [ 123 ] 
    return $
      map (\(Only poly) -> poly ) xs




main = do
    conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

    xs <- search conn "argo & profiles"

    print xs

    print "done"

