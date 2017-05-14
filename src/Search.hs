
{-# LANGUAGE OverloadedStrings #-}

module Search where

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Database.PostgreSQL.Simple.Internal as Internal(Connection)
-- import qualified Database.PostgreSQL.Simple.Internal(Conection(..)) -- as Internal( Connection(..) )
-- import Database.PostgreSQL.Simple.Types as PG(Only(..))

import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as LT(putStrLn)
import qualified Data.Text.Lazy as LT

import Debug.Trace(trace)
import Data.Function( (&) )

import Control.Monad(unless, when)

-- should put this in a module TestConceptRecords - 

import qualified FacetCalc as FacetCalc --(buildLeafFacetMap,main)
import qualified Summary as Summary--(main)


mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e





-- this is monadic
request :: Connection -> IO LT.Text

request conn = do 

  let trace_ = False

  -- TODO - maybe put all the DB actions into another file -- so there's a clear module interface...
  -- TODO - control logging in a switch
  -- change to getNestingFromDB
  -----------------------
  -- get stuff parent/child nestings
  -- is this a fast lookup, should we move this out of the facet code...
  nestings <- FacetCalc.getConceptNesting conn 
  -- if trace_ then mapM print nestings else return [ ]
  -- case trace_ of True -> mapM print nestings 
  -- when trace_ $ ( mapM print nestings >> return ())


  facetLeafCounts <- FacetCalc.getConceptRecordList conn
  -- print "##### the facetLeaf counts "
  -- mapM print facetLeafCounts


  -- compute facet counts
  let facetCounts = FacetCalc.buildInitialConceptMap facetLeafCounts
  -- print "##### the facetCounts after creating the leaf map "
  -- (mapM print).(Map.toList) $ facetCounts


  let (propagated, allRecordIds) = FacetCalc.doAll nestings  facetCounts
  -- print "##### the facetCounts after propagating"
  -- (mapM print).(Map.toList) $ propagated

  print $ allRecordIds 
 

  -- get the concept, parents and labels from db as a Map
  let makePair (concept, parent, label) = 
        (Just concept, (parent, label))  -- turn into key,val pairs needed for map,

  labels <- FacetCalc.getConceptLabels conn 
      >>= return.(Map.fromList).(map makePair)
      -- >>= return.(\m ->  Map.insert Nothing ( Nothing, "this si wrong ") m )  -- insert a root node -- this isn't right

  -- print "##### labels"
  -- (mapM print).(Map.toList) $ labels


  -- now join the label information with the facet list
  -- TODO propagated should be passed as an argument
  let completeConceptRecordList = 
       Map.foldlWithKey f [] propagated 
        where
        f m concept (count, records) = 
          let (parent, label) = mapGet concept labels in
            case concept of
              Nothing -> 
                {-
                  the root node, which appears once - and is'nt a concept or something that we have a label
                  and we cannot store the parent parent_id which will be Nothing since then we get a self-referential child/parent
                  that will create infinite recursion when we go to format the graph.
                -}
                -- trace ("this is the rootNode count " ++ show count ) $ 
                m
              Just concept_id ->
                -- a normal concept
                (concept, parent, label, count) : m

  -- print "##### complete facet list"
  -- (mapM print) completeConceptRecordList


  -- build the graph for output formatting 
  let facetGraph = Summary.fromList completeConceptRecordList
  -- (mapM print).(Map.toList) $ facetGraph


  let sortedGraph = Summary.sort facetGraph
  -- (mapM print).(Map.toList) $ facetGraph

  -- format the thing -
  -- ConceptRecordFormat.printXML (length allRecordIds) sortedGraph

  let s = Summary.formatXML (length allRecordIds) sortedGraph

  return s



-- request :: IO String 
main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  s <- request conn

  LT.putStrLn $ s

  return ()


