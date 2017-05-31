
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes #-}

module LoadImage where


import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import Text.RawString.QQ
-- import qualified Data.ByteString.Char8 as BS(putStrLn, concat)
-- import qualified Data.ByteString.Char8 as BS(ByteString(..), pack, putStrLn, concat, readFile)
import qualified Data.ByteString.Char8 as BS(ByteString(..), readFile)

import qualified Config as Config(connString)


{-
  create table x ( id serial   primary key not null  , image bytea ) ;
-}

{-
  https://hackage.haskell.org/package/postgresql-simple-0.5.0.0/candidate/docs/Database-PostgreSQL-Simple.html#g:14
  "The String and Text types are assumed to be encoded as UTF-8."

-}

-- chnage name ImageStore ImageGet or Image.get  Image.store ?

storeImage conn s = do
    xs :: [(Only Int)] <- PG.query conn
        [r|
            insert into x(image)
            values (?)
            returning id
        |]
        -- NOTE - the use of Binary type constructor, to avoid UTF-8
        $ Only $ Binary (s :: BS.ByteString )

    let record_id = case xs of
         [ Only record_id ] -> Just record_id
         _ -> Nothing

    putStrLn $ "record_id is " ++ show record_id
    return record_id






main = do

    s <- BS.readFile "resources/logo.png"

    -- print s

    conn <- PG.connectPostgreSQL Config.connString

    storeImage conn s

    PG.close conn

