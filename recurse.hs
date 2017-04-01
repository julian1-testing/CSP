
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}


import Database.PostgreSQL.Simple

import Text.RawString.QQ


pad s count =
  case count == 0 of
    True -> s
    False -> pad (" " ++ s) (count - 1)


recurse conn depth (parent_id, label) = do
  putStrLn $ (pad "" $ depth * 3) ++ "- " ++ show parent_id ++ " " ++ label

  let query1 = [r|
        select id, label
        from concept_view
        where parent_id = ?
  |]
  xs :: [ (Integer, String) ] <- query conn query1 (Only parent_id)
  mapM (recurse conn $ depth + 1) xs
  return ()



main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  let query1 = [r|
        select id, label
        from concept_view
        where parent_id is null
        and scheme_title ~ ?
  |]

  -- let url = "Platform"  :: String
  let url = "Parameter"  :: String

  xs :: [ (Integer, String) ] <- query conn query1 (Only url)

  mapM (recurse conn 0) xs

  return ()




