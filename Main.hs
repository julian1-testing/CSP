
{-# LANGUAGE OverloadedStrings #-}

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)

import qualified Facet as Facet--(buildLeafFacetMap,main)
import qualified FacetFormat as FacetFormat--(main)

import qualified Data.Map as Map

import Debug.Trace(trace)


import Data.Function( (&) )


mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e





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
  -- print "##### the facetLeaf counts "
  -- mapM print facetLeafCounts


  -- compute facet counts
  let facetCounts = Facet.buildLeafFacetMap facetLeafCounts
  -- print "##### the facetCounts after creating the leaf map "
  -- (mapM print).(Map.toList) $ facetCounts


  let (propagated, allRecordIds) = Facet.doAll nestings  facetCounts
  -- print "##### the facetCounts after propagating"
  -- (mapM print).(Map.toList) $ propagated

 

  -- get the concept, parents and labels from db as a Map
  let makePair (concept, parent, label) = 
        (Just concept, (parent, label))  -- turn into key,val pairs needed for map,

  labels <- Facet.getConceptLabels conn 
      >>= return.(Map.fromList).(map makePair)
      -- >>= return.(\m ->  Map.insert Nothing ( Nothing, "this si wrong ") m )  -- insert a root node -- this isn't right

  -- print "##### labels"
  -- (mapM print).(Map.toList) $ labels

  -- now join the label information with the facet list
  let completeFacetList = 
       Map.foldlWithKey f [] propagated 
        where
        f m concept (count, records) = 

          let (parent, label) = mapGet concept labels in
            case concept of
              Nothing -> 
                -- the root node, which appears once - and is'nt a concept that we have a label for 
                trace ("this is the rootNode count " ++ show count ) $ 
                m
              Just concept_id ->
                -- a normal concept
                (concept, parent, label, count) : m



  -- print "##### complete facet list"
  -- (mapM print) completeFacetList

  -- we should be adding a root node here...
  -- VERY IMPORTANT - the root should be created in the facetFormat.fromList 

  -- build the graph for output formatting 
  let facetGraph = FacetFormat.fromList completeFacetList
  -- (mapM print).(Map.toList) $ facetGraph

  -- are we really sure we need to pass the root explicitly 

{-
  -- Nothing?????/
  let rootNode = mapGet Nothing facetGraph
  print "rootNode"
  print rootNode
  -- root node is an array?  why????
-}

  let sortedGraph = FacetFormat.sort facetGraph
  -- (mapM print).(Map.toList) $ facetGraph


  -- format the thing -
  FacetFormat.printXML sortedGraph



  return () 




{-
          case concept of 
            -- ignore the root node
            Nothing -> m
            -- otherwise just insert, zipping with parent_id and label  
            Just concept_id ->

                  let (parent, label) = mapGet labels concept_id in
                  (concept_id, parent, label, count) : m
-}


