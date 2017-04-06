
{-
  eg. generate this,

https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes #-}


module FacetFormat where


import qualified Data.Map as Map
import qualified Data.List as List(sortOn, unfoldr)
import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Data.Function( (&) )

import Text.RawString.QQ


import qualified Facet as Facet


-- ease syntax
mapGet = (Map.!)


-- generate a white space String with length of count
pad count = List.unfoldr f count
  where f x = case x of
          0 -> Nothing
          _ -> Just (' ', x - 1)


-- TODO
-- a dummy root node - because we use it in multiple places - but there's a typing issue somewhere,
-- rootNode = (Nothing, "dummy", -999  )



fromList xs =
  -- Map of concept_id -> [ child concepts ]
  -- store the concept/child relationships in a map to enable O(log n) lookup of a nodes children

  Map.empty
  & \m -> Map.insert Nothing [] m    -- add a root/parent node
  & \m -> foldl createEmptyList m xs
  & \m -> foldl insertToList m xs

  where
    -- insert empty list for concept_id
    -- this is ok.
    createEmptyList m (concept_id,_,_,_) =
      Map.insert (Just concept_id) [] m

    -- add items to the list associated with graph node with children
    insertToList m (concept_id, parent_id, label,count) =
      let childLst = mapGet m parent_id in
      let newChildren = (concept_id, label, count) : childLst in
      Map.insert parent_id newChildren m




sort m =
  -- TODO move to Facet?
  -- sort the children according to their count
  Map.mapWithKey f m
  where
  f k children =
    reverse. List.sortOn (\(_, _, count) -> count) $ children





print m = do
  -- non xml view of the graphoutput
  let rootNode = (Nothing, "dummy", -999 )
  -- recurse from the root node down...
  recurse m rootNode 0
  where
    -- this just prints everything and is monadic
    recurse m (parent_id, label, count) depth = do
      printRow (parent_id, label, count) depth

      -- continue recursion
      let children = mapGet m parent_id
      mapM (\(concept_id, label, count) -> recurse m (Just concept_id, label, count) (depth + 1)) children
      return ()

    printRow (parent_id, label, count) depth  = do
      putStrLn $ concatMap id [ (pad $ depth * 3), (show parent_id), " ",  (show label), " ", (show count) ]



printXML m = do
  -- should change this so that it's returning a string? or concatenating?

  -- we will recurse from the root node down...
  let rootNode = (Nothing, "dummy", -999 )
  recurse m rootNode  0
  where
    -- recurse down into child nodes
    recurse m (parent_id, label, count) depth = do

      -- what's going on here?
      case label of 

        "AODN Parameter Category Vocabulary" -> do
          putStrLn "whoot"
          return ()

        _ -> do
          doCategory (parent_id, label, count) depth 


    processChildren (parent_id, label, count) depth = do 
        -- get the children of this node and process them
        let children = mapGet m parent_id
        mapM (\(concept_id, label, count)  -> recurse m (Just concept_id, label, count) (depth + 1)) children --sortedChildren


{-
    doDimension (parent_id, label, count) depth = do 
      putStrLn $ concatMap id [
        (pad $ depth * 3),
        "<dimension value=\"", label, "\"", " count=", show count, " >"
        ]
        -- don't have to drill into the children at all,

        {- dimensionStartTag a  depth
        processChildren a depth
        dimensionEndTag a depth -}
        return ()
-}

    doCategory a depth  = do
        -- do category start tag
        categoryStartTag a  depth
        processChildren a depth
        categoryEndTag a depth
        return ()


    categoryStartTag (parent_id, label, count) depth  = do
      putStrLn $ concatMap id [
        (pad $ depth * 3),
        "<category value=\"", label, "\"", " count=", show count, " >"
        ]

    categoryEndTag (parent_id, label, count) depth = do
      putStrLn $ concatMap id [
        (pad $ depth * 3),
        "</category>"
        ]



getTestFacetList conn = do
  -- get all facets and facet count from db and return as flat list
  let query = [r|
        select
          id as concept_id,
          parent_id,
          label ,
          count
        from facet_view
  |]
  xs :: [ (Integer, Maybe Integer, String, Integer) ] <- PG.query conn query ()
  return xs



main :: IO ()
main = do

  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  facetList <- getTestFacetList conn

  let  facetList' = map (\f (a,b,c, d) -> (a,b,c, 123) )  facetList
  let  facetList' = facetList

  let m = fromList facetList'

  FacetFormat.print m

  -- nestings <- Facet.getConceptNesting conn
  -- let m' =  Facet.propagateAllRecordsToRoot nestings m

  let m' = sort m

  FacetFormat.print m'

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

fromList' facetMap =

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

