
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

  mapping from concept_id -> children

  I think we will create another one 

  concept_id -> counts

  therefore we should remove the count from here.
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

    -- TODO - do this explicitly for the root node
    -- insert empty list for parent_id
    addParentId m (_,parent_id,_,_) =
      Map.insert (parent_id) [] m

    -- populate the list associated with graph node with children
    insertToList m (concept_id, parent_id, label,count) =
      let childLst = mapGet m parent_id in
      let newChildren = (concept_id, label, count) : childLst in
      Map.insert parent_id newChildren m

{-
  Important - after calculating creating thie . we can probably go back and map the list again... 
      and just update the counts....

  Ok, so we can recurse the graph... although it's pretty har
  
  it should be pretty easy if we just return the damn count each time.... 
  and store it...
      done.
-}    

{-
    So rather than store it directly we should create another Map from concept 
-}

recurseFacetGraph g =
  let rootNode = (Nothing, "dummy", -999) in
  recurse g rootNode  0 
  where
    recurse g (parent_id, label, count) depth =
      -- we have to drill/ recurse  down first
      let children = mapGet g parent_id in

      -- we can't use recurse here....
      -- recurse either returns a list. or it returns a graph.
      -- i think it wants to be a fold
      

      let f (concept_id, label, count) = recurse g (Just concept_id, label, count) (depth + 1) in
      let newChildren = map f children in
      Map.insert parent_id [ ] g
      

      -- it has to be an empty graph -- but that makes concatenating too difficult?
      -- foldl      children 
 

      -- remove item
      -- insertNew item with children...
      -- and because it's so damn hard... 
      -- i don't think a map will 
      -- 
      -- let sumCount = foldl (\sum (a,b,c) -> sum + c) 0 children in 
      -- ()





-- 1. build a level - from the facetGraph

buildFacetCounts xs =
  -- iteratively go through the list and create facet counts.
  -- but this doesn't work - unless we do leaf nodes then other nodes etc in order
  -- eg. have to do all the nodes at a particular level before we consider the next level...
  -- holy hell.
  -- introduce a recursion level.... and propagate them up?

  -- OK, but with the graph - we have the depth, level. so we just need to transform to the level...

  -- let e' = foldl addParentId Map.empty xs in
  let e' = Map.empty in

  let e'' = Map.insert (Nothing) 0 e' in -- add entry for root
  let e  = foldl addConceptId e'' xs in
  foldl insertToList e xs
  where
    -- setting the thing to 0 is not correct for the leaf nodes...
    -- add 0 for concept_id
    addConceptId m (concept_id,_,_,_) =
      Map.insert (Just concept_id) 0 m

    -- add 0 for parent_id
--    addParentId m (_,parent_id,_,_) =
--      Map.insert (parent_id) 0 m

    -- populate the list associated with graph node with children
    insertToList m (concept_id, parent_id, label, count) =
      let parentCount = mapGet m parent_id in
      Map.insert parent_id (parentCount + count)  m





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


  let facetCounts = buildFacetCounts facetList

  print facetCounts

  let facetGraph = buildFacetGraph facetList


  -- let x = recurseFacetGraph facetGraph 

  printFacetGraph facetGraph 

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


