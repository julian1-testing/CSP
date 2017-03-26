{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple

main = hello

hello :: IO Int
hello = do
  conn <- connectPostgreSQL "host='dbprod.emii.org.au' dbname='harvest' user='jfca' sslmode='require'"
  [Only i] <- query_ conn "select 2 + 2"
  print i
  return i


