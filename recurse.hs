
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

{-
    - we need to recurse twice - once to propagate the counts up the tree nodes. 
    - this should be non monadic...

-}

getAllFacets conn  = do
  -- get all facets and facet count from db and return as flat list
  let query1 = [r|
        select 
          concept_id, 
          parent_id,
          label,
          count
        from facet_count_view2
  |]
  -- note the parent may be null! beautiful...
  xs :: [ (Integer, Maybe Integer, String, Integer ) ] <- query conn query1 ()
  -- mapM print xs
  return xs


-- eg.
-- let physicalWaterChildren = mapGet facetMap (Just 583) -- eg. physical water
-- recurse facetMap (Just 583) 0


mapGet = (Map.!)

{-
  VERY IMPORTNAT
  IT HAS TO BE A FLAT MAP - because it's a graph not a tree
-}

buildFacetGraph :: Foldable t =>
     t (Integer, Maybe Integer, t1, t2)
     -> Map.Map (Maybe Integer) [(Integer, t1, t2)]

buildFacetGraph xs =
  -- takes a 
  -- build a graph of the facet nodes
  -- non monadic
  -- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html
  let e' = foldl addParentId Map.empty xs in
  let e  = foldl addConceptId e' xs in
  foldl insertToList e xs

  where
    -- insert empty list for concept_id
    addConceptId m (concept_id,_,_,_) =
      Map.insert (Just concept_id) [] m

    -- insert empty list for parent_id
    addParentId m (_,parent_id,_,_) =
      Map.insert (parent_id) [] m

    -- populate the list associated with graph node with children
    insertToList m (concept_id, parent_id, label,count) =
      let childLst = mapGet m parent_id in
      let newChildren = (concept_id, label, count) : childLst in
      Map.insert parent_id newChildren m




recurseFacetGraph g =

  let rootNode = (Nothing, "dummy", -999) in
  recurse g rootNode 

  where
  recurse g (parent_id, label, count) = 

    let children = mapGet g parent_id in
    -- let newChildren = map (\(a,b,c) -> (a,b,c + 1000)) children in

    foldl (\g' (a,b,c) -> 
        -- we have to modify g here
        let newChild = (a,"whoot" ++ b, c) in
  
        -- let newGraph = Map.insert (Just parent_id) newChild  g' in
        let newGraph = recurse g' (Just a, b,c) in
  
        newGraph

    ) g children 

  -- this isn't fucking woking at all...

    -- this is recursing but not creating a new map....


-- this is fucking complicated.
-- we don't actually have the recurse working ... 
-- ok, uggh, the node doesn't actually have count associated with it....

{-
      -- putStrLn $ concatMap id [ (pad $ depth * 3), (show parent_id), " ",  (show label), " ", (show count) ]
      -- g is a Map, key Maybe Int, and the value  is a list - of other maps
      let children = mapGet g parent_id in

      -- i think we just need to remove the element... 
      -- and then append
      -- this isn't a list... or is it?
      -- this is a list...
      let x = map (\(concept_id, label, count)  -> recurse g (Just concept_id, label, count) ) children in
      -- so we need to keep passing a new map down and inserting elements into it...
      -- let _ = map ( recurse g ) children in
      let g1 = foldl (m c -> Map.insert   g1 children in
-}





printFacetGraph g = do
  let rootNode = (Nothing, "dummy", -999  )
  recurse g rootNode  0 
  where
    -- this just prints everything and is monadic
    recurse g (parent_id, label, count) depth = do

      putStrLn $ concatMap id [ (pad $ depth * 3), (show parent_id), " ",  (show label), " ", (show count) ]

      let children = mapGet g parent_id

      mapM (\(concept_id, label, count)  -> recurse g (Just concept_id, label, count) (depth + 1)) children
      return ()



-- need mineral water

main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  facetList <- getAllFacets conn

  let facetGraph = buildFacetGraph facetList


  let x = recurseFacetGraph facetGraph 

  printFacetGraph x 

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


