
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}


import Database.PostgreSQL.Simple

import Text.RawString.QQ

import qualified Data.Map as Map

-- https://www.reddit.com/r/haskell/comments/4gmw1u/reverse_function_application/
import Data.Function( (&) )


-- ok,


pad' s count =
  case count == 0 of
    True -> s
    False -> pad' (" " ++ s) (count - 1)


pad count = pad' "" count




getFacetList conn  = do
  -- get all facets and facet count from db and return as flat list
  let query1 = [r|
        select 
          id as concept_id, 
          parent_id,
          label,
          -- node_count as count
          -- count_sum as count
          count 
        from facet_count_view
  |]
  -- note the parent may be null! beautiful...
  xs :: [ (Integer, Maybe Integer, String, Integer ) ] <- query conn query1 ()
  -- mapM print xs
  return xs


mapGet = (Map.!)



buildFacetGraph :: Foldable t =>
     t (Integer, Maybe Integer, t1, t2)
     -> Map.Map (Maybe Integer) [(Integer, t1, t2)]



buildFacetGraph xs =
  -- stores nesting relationships in a map to enable easy lookup
  -- concept_id -> array 
  -- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html

  Map.empty
  & \m -> Map.insert Nothing [] m    -- insert a root node
  & \m -> foldl emptyList m xs
  & \m -> foldl insertToList m xs

  where
    -- insert empty list for concept_id
    emptyList m (concept_id,_,_,_) =
      Map.insert (Just concept_id) [] m

    -- add items to the list associated with graph node with children
    insertToList m (concept_id, parent_id, label,count) =
      let childLst = mapGet m parent_id in
      let newChildren = (concept_id, label, count) : childLst in
      Map.insert parent_id newChildren m



printFacetGraph m = do
  -- we will recurse from the root node down...
  let rootNode = (Nothing, "dummy", -999  )
  recurse m rootNode  0 
  where
    -- this just prints everything and is monadic
    recurse m (parent_id, label, count) depth = do

      printRow (parent_id, label, count) depth  

      -- continue recursion
      let children = mapGet m parent_id
      mapM (\(concept_id, label, count)  -> recurse m (Just concept_id, label, count) (depth + 1)) children
      return ()

    printRow (parent_id, label, count) depth  = do
      putStrLn $ concatMap id [ (pad $ depth * 3), (show parent_id), " ",  (show label), " ", (show count) ]





printXMLFacetGraph m = do
  -- we will recurse from the root node down...
  let rootNode = (Nothing, "dummy", -999  )
  recurse m rootNode  0 
  where
    -- this just prints everything and is monadic
    recurse m (parent_id, label, count) depth = do

      printRow (parent_id, label, count) depth  

      -- continue recursion
      let children = mapGet m parent_id
      mapM (\(concept_id, label, count)  -> recurse m (Just concept_id, label, count) (depth + 1)) children
      return ()

    printRow (parent_id, label, count) depth  = do
      putStrLn $ concatMap id [ 
        (pad $ depth * 3), 
        "<dimension ",  
        (show parent_id), 
        " ",  (show label), " ", (show count) 
        ]





-- need mineral water

main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  facetList <- getFacetList conn

  let m = buildFacetGraph facetList
  -- printFacetGraph m 
  printXMLFacetGraph m

  return ()









{-
  let depthMap = buildDepthMap g 

  mapM print (Map.toList depthMap) 

  let zipped = zipFacetListWithDepth facetList depthMap

  mapM print zipped
-}



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

{-
------
---- we're not thinking ....
--- our structure hasn't been returned 

buildFacetGraph' facetMap =

  recurse' facetMap (Nothing, "dummy", -999  ) 0

  where
  recurse' m (parent_id, label, count) depth = 

    -- look up the children...
    -- hmmmm we are going to have to generate a new map?
    let children = mapGet m parent_id in

    let f (concept_id, label, count) = recurse' m (Just concept_id, label, count) (depth + 1) in

    map f children

-}

{-
  ok, so we have the map...  lets try to now we want to transform the original list 

  append is modify
  where is

  IMPORTNAT
  TODO change name depth to nestingLevel 
-}

{-
zipFacetListWithDepth xs depthMap = 
  sortOn  (map f) $ xs
  
  where
    compare (a,b,c,d,e) (a,b,c,d,e) = 
    f (a,b,c,d) = (a,b,c,d, mapGet depthMap (Just a))
-}


{-

buildDepthMap g =
  -- this is a recursion
  -- build a Map from Graph indexed by concept id with depth of the facet
  let rootNode = (Nothing, "dummy", -999) in
  recurse g Map.empty rootNode 0 
  where
    recurse g depthMap (parent_id, label, count) depth =
      -- set depth for this current id,
      let depthMap' = Map.insert parent_id depth depthMap in
      -- get children and drill 
      let children = mapGet g parent_id in
      -- recurse/process the children
      let f depthMap' (concept_id, label, count) = recurse g depthMap' (Just concept_id, label, count) (depth + 1) in
      foldl f (depthMap') children
-}


{-
  VERY IMPORTNAT
  IT HAS TO BE A FLAT MAP - because it's a graph not a tree

  mapping from concept_id -> children

  I think we will create another one 

  concept_id -> counts

  therefore we should remove the count from here.
-}
{-
    - we need to recurse twice - once to propagate the counts up the tree nodes. 
    - this should be non monadic...

-}
{-
-- using multiple db queries to extract the tree is slow - quicker to get everything flat and then destructure to a tree
-- BUT - first - we need to get the counts being returned and then propagating up

-- So it will always require a custom query....
-- also we need to be returning for all vocab not just parameter


-- IMPORTANT
-- are we sure we cannot use a groupby on the facet_count_view to get the counts ...
-- this would make client side stuff a lot simpler.
-}

