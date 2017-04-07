
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
import Debug.Trace(trace)

import Text.RawString.QQ


import qualified Facet as Facet


import qualified Data.Text.Lazy as LT

-- ease syntax
mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e




-- generate a white space String with length of count
pad count = List.unfoldr f count
  where f x = case x of
          0 -> Nothing
          _ -> Just (' ', x - 1)


-- TODO
-- a dummy root node - because we use it in multiple places - but there's a typing issue somewhere,
-- rootNode = (Nothing, "dummy", -999  )

-- We have no way of representing the root node...
-- so we will have to pass it explicitly...

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
      Map.insert (concept_id) [] m

    -- add items to the list associated with graph node with children
    insertToList m (concept_id, parent, label,count) =

        let childLst = mapGet parent m in
        let newChildren = (concept_id, label, count) : childLst in
        Map.insert parent newChildren m


{-
      case parent of 
        Nothing -> m
          -- this is where we have to 
          -- why doesn't this work if there is a root node there already

        Just concept_id ->
          let childLst = mapGet parent m in
          let newChildren = (concept_id, label, count) : childLst in
          Map.insert parent newChildren m

-}


sort m =
  -- TODO move to Facet?
  -- sort the children according to their count
  -- should sort before adding labels to tuples?
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
      let children = mapGet parent_id m 
      mapM (\(concept_id, label, count) -> recurse m (Just concept_id, label, count) (depth + 1)) children
      return ()

    printRow (parent_id, label, count) depth  = do
      putStrLn $ concatMap id [ (pad $ depth * 3), (show parent_id), " ",  (show label), " ", (show count) ]



-- so all we need to do is pass the actual root node in here explicitly 
-- VERY IMPORTANT - it might be possible to do this more simply - by having separate lists. 

-- lazy string concat
myConcat lst = foldl LT.append LT.empty lst

-- myConcat lst = concatMap LT.append lst




formatXML rootRecordCount m = 

  {-  we will recurse from the root node down...
      remember that we cannot have a Nothing node above the Nothing node. so there's nowhere else to store label or count 
      information which isn't a known concept or scheme anyway
  -}
  let rootNode = (Nothing, "summary", rootRecordCount ) in
  recurse m rootNode 0
  where
    -- recurse down into child nodes
    recurse m (parent_id, label, count) depth = 

      -- what's going on here?
      case label of 
        -- "summary" ->

        -- should we be doing this label substitution here? or when loading the vocab scheme and set the label? ...
        "AODN Parameter Category Vocabulary" ->
          doDimension (parent_id, "Measured Parameter", count) depth 

        "AODN Platform Category Vocabulary" ->
          doDimension (parent_id, "Platform", count) depth 

        _ ->
          doCategory (parent_id, label, count) depth 

    doSummary (parent_id, label, count) depth  = 
      myConcat [
          LT.pack $ (pad $ depth * 3),
          LT.pack $ mconcat [ "<summary count=", show count, " type=\"local\"/>" ],
          processChildren (parent_id, label, count) depth
      ]



    doDimension (parent_id, label, count) depth =
      -- single closed tag...
        myConcat [
            LT.pack $ (pad $ depth * 3),
            LT.pack $ mconcat [ "<dimension value=\"", label, "\"", " count=", show count, " />\n"  ], 
            processChildren (parent_id, label, count) depth
        ]

    doCategory (parent_id, label, count) depth =
     
      myConcat [ 
        -- start tag
          LT.pack $ (pad $ depth * 3),
          LT.pack $ mconcat [ "<category value=\"", label, "\"", " count=", show count, " >\n" ],
        -- children
        processChildren (parent_id, label, count) depth ,
        -- end tag
        LT.pack $ (pad $ depth * 3),
        LT.pack "</category>\n"
      ]

-- print will append a new line

    processChildren (parent_id, label, count) depth =
        -- take children
        let children = mapGet parent_id m  in
        -- recurse into children
        let f txt (concept, label, count) = txt $ recurse m (concept, label, count) (depth + 1) in
        -- fold over children appending text
        foldl (f.LT.append) LT.empty children
        -- TODO use myConcat? eg.
        -- concatMap f children

{-
printXML rootRecordCount m = do

  -- we will recurse from the root node down...
  -- remember that we cannot have a Nothing node above the Nothing node. so there's nowhere else to store label or count 
  -- information which isn't a known concept or scheme anyway
  let rootNode = (Nothing, "summary", rootRecordCount)
  recurse m rootNode 0
  where
    -- recurse down into child nodes
    recurse m (parent_id, label, count) depth = do

      -- what's going on here?
      case label of 
        -- TODO remove this....
        -- should we be doing this label substitution here? or when loading the vocab scheme and set the label? ...
        "summary" ->
          doSummary (parent_id, label, count) depth 
    
        "AODN Parameter Category Vocabulary" ->
          doDimension (parent_id, "Measured Parameter", count) depth 

        "AODN Platform Category Vocabulary" ->
          doDimension (parent_id, "Platform", count) depth 

        _ ->
          doCategory (parent_id, label, count) depth 


    processChildren (parent_id, label, count) depth = do 
        -- get the children of this node and process them
        let children = mapGet parent_id m 
        -- and process them
        mapM f children
        return ()
        where 
          f (concept_id, label, count) = recurse m ({-Just -}concept_id, label, count) (depth + 1)
          

    doSummary (parent_id, label, count) depth  = do
      -- single closed tag...
      putStrLn $ concatMap id [
        -- TODO don't use concatMap here...
        (pad $ depth * 3),
        "<summary count=", show count, " type=\"local\"/>"
        ]
      processChildren (parent_id, label, count) depth


    doDimension (parent_id, label, count) depth  = do
      -- single closed tag...
      putStrLn $ concatMap id [
        (pad $ depth * 3),
        "<dimension value=\"", label, "\"", " count=", show count, " />"
        ]
      processChildren (parent_id, label, count) depth


    doCategory (parent_id, label, count) depth  = do
        -- start tag
      putStrLn $ concatMap id [
        (pad $ depth * 3),
        "<category value=\"", label, "\"", " count=", show count, " >"
        ]
      processChildren (parent_id, label, count) depth

      -- end tag
      putStrLn $ concatMap id [
        (pad $ depth * 3),
        "</category>"
        ]

-}



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

{-
  let m = fromList facetList'

  FacetFormat.print m

  -- nestings <- Facet.getConceptNesting conn
  -- let m' =  Facet.propagateAllRecordsToRoot nestings m

  let m' = sort m

  FacetFormat.print m'
-}
  return ()






{-
        let f txt (concept, label, count) = txt $ recurse m (concept, label, count) (depth + 1) in
        -- fold over children appending text
        foldl (f.LT.append) LT.empty children
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

