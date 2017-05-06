{-
  
  Format the result of xml.search.imos, used for Facets

https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes #-}


module FacetFormat where


import qualified Data.Map as Map
import qualified Data.List as List(sortOn, unfoldr)
import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import qualified Data.Text.Lazy as LT(pack, empty, append)
import Data.Function( (&) )
import Debug.Trace(trace)

import Text.RawString.QQ


import qualified FacetCalc as FacetCalc



-- ease syntax
mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e




-- generate a white space String with length of count
pad count = 
  LT.pack $ List.unfoldr f count
    where f x = case x of
            0 -> Nothing
            _ -> Just (' ', x - 1)



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



sort m =
  -- TODO move to Facet?
  -- sort the children according to their count
  -- should sort before adding labels to tuples?
  Map.mapWithKey f m
  where
  f k children =
    reverse. List.sortOn (\(_, _, count) -> count) $ children


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
    -- drill into child nodes
    recurse m (parent, label, count) depth = 

      -- what's going on here?
      case label of 
        "summary" ->
          outputSummary (parent, label, count) depth 

        -- should we be outputing this label substitution here? or when loading the vocab scheme and set the label? ...
        "AODN Parameter Category Vocabulary" ->
          outputDimension (parent, "Measured Parameter", count) depth 

        "AODN Platform Category Vocabulary" ->
          outputDimension (parent, "Platform", count) depth 

        _ ->
          outputCategory (parent, label, count) depth 

    outputSummary (parent, label, count) depth  = 
      myConcat [
          pad $ depth * 3,
          "<", LT.pack label, " count=\"", LT.pack.show $ count, "\" type=\"local\">\n",
          outputChildren (parent, label, count) depth
      ]


    outputDimension (parent, label, count) depth =
      -- single closed tag...
      myConcat [
          pad $ depth * 3,
          "<dimension value=\"", LT.pack label, "\" count=\"", LT.pack.show $ count, "\">\n", 
          outputChildren (parent, label, count) depth
      ]


    outputCategory (parent, label, count) depth =
      myConcat [ 
        -- start tag
        pad $ depth * 3,
        "<category value=\"", LT.pack label, "\" count=\"", LT.pack.show $ count, "\">\n",
        -- children
        outputChildren (parent, label, count) depth ,
        -- end tag
        pad $ depth * 3,
        "</category>\n"
      ]


    outputChildren (parent, label, count) depth =
        -- take children
        let children = mapGet parent m  in
        -- recurse into children
        let f acc (concept, label, count) = acc $ recurse m (concept, label, count) (depth + 1) in
        -- fold over children appending text
        foldl (f.LT.append) LT.empty children
        -- TODO use myConcat? eg.
        -- concatMap f children



getTestFacetList conn = do
  -- get all facets and facet count from db and return as flat list
  let query = [r|
        select
          concept_id,
          parent_id,
          label ,
          count
        from facet_view
  |]
  xs :: [ (Integer, Maybe Integer, String, Integer) ] <- PG.query conn query ()
  return xs



{-
  Main test/example is in FacetRequest


-}


