
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}


import Database.PostgreSQL.Simple

import Text.RawString.QQ

import qualified Data.Map as Map

-- rather than doing multiple db queries - it may be easier to just load everything into memory and then
-- query. 
-- BUT - first - we need to get the counts being returned and then propagating up

-- So it will always require a custom query....
-- also we need to be returning for all vocab not just parameter

-- IMPORTANT
-- getting everything in one query is fast - 20ms
-- lets try to join on the counts

-- IMPORTANT
-- are we sure we cannot use a groupby on the facet_count_view to get the counts ...
-- this would make client side stuff a lot simpler.


pad s count =
  case count == 0 of
    True -> s
    False -> pad (" " ++ s) (count - 1)



getAllConcepts conn  = do
  -- get facets with record count as flat list from db
  let query1 = [r|
        select 
          concept_id, 
          count,
          label,
          parent_id
        from facet_count_view2
  |]
  -- note the parent may be null! beautiful...
  xs :: [ (Integer, Integer, String, Maybe Integer) ] <- query conn query1 ()

  -- mapM print xs

  -- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html
  let emptyMap     = foldl insertEmptyList Map.empty xs
  let populatedMap = foldl insertToList emptyMap xs

  -- eg.
  let physicalWaterChildren = mapGet populatedMap (Just 583) -- eg. physical water
  putStrLn $ show physicalWaterChildren

  -- let topConcepts = mapGet populatedMap (Nothing) -- toplevel concepts - that have no pareh. 
  -- putStrLn $ show $ topConcepts  -- toplevel concepts - that have no pareh. 

  -- 
  recurse populatedMap (Just 583) 


  return ()
  where
    mapGet = (Map.!)
    -- insert key=parent_id, value=empty list
    insertEmptyList m (concept_id,_,_, parent_id) =
      Map.insert (Just concept_id) [] m

    -- insert key=parent_id, and const the concept_id to the list
    insertToList m (concept_id,count,label, parent_id) =
      let children = (Map.!) m parent_id in
      let newChildren = concept_id : children in
      Map.insert parent_id newChildren m

    -- this is going to be monadic... needs depth....
    recurse m parent_id = do
      putStrLn $ show parent_id
      let children = mapGet m parent_id

      -- ok, an entry - may not have any children - in which case 

      -- this call isn't correct
      -- hang on.... why is the element not there
      mapM (\e -> recurse m $ Just e) children
      return ()



main :: IO ()
main = do


-- ok, - now I think we want to recurse... do we have an order or something ? 


{-
  let mapInsert m (k,v) = Map.insert k v m
  let m = foldl  mapInsert Map.empty [ (123, 456) ] 
  putStrLn $ show $ (Map.!) m 123 
-}

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"


  getAllConcepts conn

{-
  let query1 = [r|
        select id, label
        from concept_view
        where parent_id is null
  |]

        -- and scheme_title ~ ?

  -- let url = "Platform"  :: String
  let url = "Parameter"  :: String

  xs :: [ (Integer, String) ] <- query conn query1 () -- (Only url)

  mapM (recurse conn 0) xs
-}
  return ()


{-

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


-}
