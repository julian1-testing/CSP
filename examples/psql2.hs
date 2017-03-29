{-# LANGUAGE OverloadedStrings #-}


-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables #-}

import Database.PostgreSQL.Simple

main = hello

-- hello :: IO Int
hello = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  xs :: [ (Int, String) ] <- query_ conn "select id, url from catalog"

  let formatRow (id,url) = foldr (++) "" [ show id, " ",  url ]

  mapM  (putStrLn.formatRow)  xs


  -- Note also executeMany 
  -- _ <- query_ conn "begin;" 
  execute conn "begin;"  ()

  execute conn "insert into catalog(url) values (?)" [("catalog3" :: String ) ]

  execute conn "commit;"  ()

  return ()



  -- mapM  (\a -> putStrLn $ formatRow a ) xs
  -- mapM (\(id,url) -> putStrLn $ show id ++ ", " ++ url) xs

  -- [ Only i, j ] <- query_ conn "select id,url from catalog"
  -- [ (i, j) ] <- query_ conn "select id,url from catalog"
  -- xs <- query_ conn "select id,url from catalog"
  -- print (xs :: [Only Int])

  -- [Only i :: Integer] <- query_ conn "select id from catalog"
  -- xs :: [ (Int, Int) ] <- query_ conn "select id, url from catalog"
  -- print (xs :: Only Int  ::  Only String ])
  -- conn <- connectPostgreSQL "host='dbprod.emii.org.au' dbname='harvest' user='jfca' sslmode='require'"
  -- [Only i] <- query_ conn "select 2 + 2"

