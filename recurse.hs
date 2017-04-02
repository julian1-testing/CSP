
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}


import Database.PostgreSQL.Simple

import Text.RawString.QQ

import qualified Data.Map as Map

{-
-- using multiple db queries to extract the tree is slow - quicker to get everything flat and then destructure to a tree
-- BUT - first - we need to get the counts being returned and then propagating up

-- So it will always require a custom query....
-- also we need to be returning for all vocab not just parameter


-- IMPORTANT
-- are we sure we cannot use a groupby on the facet_count_view to get the counts ...
-- this would make client side stuff a lot simpler.
-}


pad' s count =
  case count == 0 of
    True -> s
    False -> pad' (" " ++ s) (count - 1)


pad count = pad' "" count


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
  let emptyMap'     = foldl initParents Map.empty xs
  let emptyMap     = foldl initConcepts emptyMap' xs

  let populatedMap = foldl insertToList emptyMap xs

  -- eg.
  -- let physicalWaterChildren = mapGet populatedMap (Just 583) -- eg. physical water

  -- recurse populatedMap (Just 583) 0
  recurse populatedMap (Nothing, "dummy" ) 0

  -- it's not a map - its actually a graph

  return ()
  where
    mapGet = (Map.!)

    initParents m (_,_,_, parent_id) =
      Map.insert (parent_id) [] m

    initConcepts m (concept_id,_,_,_) =
      Map.insert (Just concept_id) [] m

    -- insert key=parent_id, and const the concept_id to the list
    insertToList m (concept_id,count,label, parent_id) =
      let children = (Map.!) m parent_id in
      let newChildren = (concept_id, label) : children in
      Map.insert parent_id newChildren m



    -- this is monadic - but we could reduce it to a tree...
    -- 
    recurse m (parent_id, label) depth = do

      putStrLn $ (pad $ depth * 3) ++ (show parent_id) ++ " " ++ (show label)
      let children = mapGet m parent_id

      -- mapM (\(concept_id, label)  -> recurse m (Just concept_id) (depth + 1)) children
      mapM (\(concept_id, label)  -> recurse m (Just concept_id, label) (depth + 1)) children
      return ()



main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  getAllConcepts conn

  return ()


{-
  let query1 = [r|
        select id, label
        from concept_view
        where parent_id is null
  |]



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
