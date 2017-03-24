{-# LANGUAGE OverloadedStrings #-}

module Psql where

import Database.PostgreSQL.Simple


main = hello

hello :: IO Int
hello = do
  -- conn <- connectPostgreSQL "host='dbprod.emii.org.au' dbname='harvest' user='jfca' sslmode='require'"
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  -- [Only i] <- query_ conn "select 2 + 2"
  [Only i] <- query_ conn "select 2 + 2"
  print i
  return i


