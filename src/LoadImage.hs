
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes #-}

module LoadImage where


import qualified Database.PostgreSQL.Simple as PG(connect, close, query, Binary(..))
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import qualified Data.ByteString.Char8 as BS(ByteString(..), readFile, writeFile)
import Text.RawString.QQ

import qualified Config as Config(connectionInfo)


{-
  create table x ( id serial   primary key not null  , image bytea ) ;

  this needs better naming. eg. Image 
  chnage name ImageStore ImageGet or Image.get  Image.store ?

  VERY IMPORTANT - can two files implement different functions of the same module?
-}


storeImage conn s = do
    xs :: [(Only Int)] <- PG.query conn
        [r|
            insert into image(image)
            values (?)
            returning id
        |]
        -- NOTE - the use of Binary type constructor, to avoid UTF-8
        -- https://hackage.haskell.org/package/postgresql-simple-0.5.0.0/candidate/docs/Database-PostgreSQL-Simple.html#g:12
        $ Only $ PG.Binary (s :: BS.ByteString )

    return $
      case xs of
         [ Only record_id ] -> Just record_id
         _ -> Nothing



getImage conn id = do
    -- xs :: [(Only Binary BS.ByteString)] <- PG.query conn
    xs :: [(Only BS.ByteString)] <- PG.query conn
        [r|
            select
              image
            from image
            where id = ?
        |]
        $ Only (id :: Int)

    return $
      case xs of
        [ Only s ] -> s
        -- else throw


getImageForUUID conn uuid = do
    -- xs :: [(Only Binary BS.ByteString)] <- PG.query conn
    -- map using same id as source
    -- join source on source.id = record.source_id
    xs :: [(Only BS.ByteString)] <- PG.query conn
        [r|
            select
              image.image
            from record
            join image on image.id = record.source_id
            where record.uuid = ?
        |]
        $ Only (uuid :: BS.ByteString)

    return $
      case xs of
        [ Only s ] -> s
        -- else throw



-- csiro.png
-- storeImage path 

storeImage' path = do
    s <- BS.readFile path
    conn <- PG.connect Config.connectionInfo
    record_id <- storeImage conn s
    putStrLn $ "stored record_id is " ++ show record_id
    s2 <- getImage conn 1   -- test retrieval
    -- BS.writeFile  "whoot.png" s2
    PG.close conn

main = do
  storeImage' "resources/imos.png"
  storeImage' "resources/csiro.png"

