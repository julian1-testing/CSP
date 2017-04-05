
{-# LANGUAGE OverloadedStrings #-}

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)

import qualified Facet as Facet--(buildLeafFacetMap,main)
import qualified FacetFormat as FacetFormat--(main)

import qualified Data.Map as Map


import Data.Function( (&) )

mapGet m e = (Map.!) m e




main :: IO ()
main = do

  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- TODO - maybe put all the DB actions into another file -- so there's a clear module interface...
  -- change to getNestingFromDB


  -----------------------
  -- get stuff parent/child nestings
  -- is this a fast lookup, should we move this out of the facet code...
  nestings <- Facet.getConceptNesting conn 
  -- mapM print nestings

  facetLeafCounts <- Facet.getFacetList conn
  mapM print facetLeafCounts


  -- compute facet counts
  let facetCounts = Facet.buildLeafFacetMap facetLeafCounts
        & Facet.propagateAllRecordsToRoot nestings 
  -- (mapM print).(Map.toList) $ facetCounts


  -- get the concept, parents and labels from db as a Map
  let makePair (concept, parent, label) = (concept, (parent, label))  -- turn into key,val pairs needed for map,
  labels <- Facet.getConceptLabels conn >>= return.(Map.fromList).(map makePair)
  -- (mapM print).(Map.toList) $ labels


  -- create a new list by zipping with label, and parent_id, and removing the root node
  -- in a form suitable for the output formatting
  let completeFacetList = 
       Map.foldlWithKey f [] facetCounts
        where
        f m concept (count, records) = case concept of 
          -- ignore the root node
          Nothing -> m
          -- otherwise just insert, zipping with parent_id and label  
          Just concept_id ->
                let (parent, label) = mapGet labels concept_id in
                (concept_id, parent, label, count) : m

  -- (mapM print) completeFacetList


  -- build the graph for output formatting 
  let facetGraph = FacetFormat.buildFacetGraph completeFacetList
  -- (mapM print).(Map.toList) $ facetGraph

 
  -- format the thing -
  FacetFormat.printXMLFacetGraph facetGraph
 
  return () 

  -- nice!!!
  -- FacetFormat.main




