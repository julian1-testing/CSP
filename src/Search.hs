{-
  the main facetted search
  Maybe change name to xmlSearchImos
-}
-- {-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Search where

import qualified Database.PostgreSQL.Simple as PG(query, connect, close)
import Database.PostgreSQL.Simple.Internal as Internal(Connection)

-- TODO Should be Data.Map.Lazy as Map ?
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as LT(putStrLn)
import qualified Data.Text.Lazy as LT

import Debug.Trace(trace)
import Data.Function( (&) )

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as E(encodeUtf8)

import qualified FacetCalc as FacetCalc --(buildLeafFacetMap,main)
import qualified Summary as Summary(fromList, sort, formatXML)
import qualified RecordGet as RecordGet(getRecords)
import qualified Metadata as Metadata(formatXML)
import qualified Helpers as H(concatLT)
import qualified Query as Query(resolveTerm)
import qualified FreeText as FreeText(search)
import qualified Config as Config(connectionInfo)

-- ease syntax
-- change to Map.lookup that returns Maybe - can then specify the error action...
mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e



printMap m =
  (mapM print).(Map.toList) $ m
  -- mapM print $ Map.toList $ m


data Params = Params {

    to :: Int,
    from :: Int,
    facetQ :: Maybe BS.ByteString,
    any :: Maybe BS.ByteString
} deriving (Show, Eq)


{-
  No we do the search - and select the propagated map.
  then trim
-}

  -- this isn't quite right - it should be parsed, and then if there is no
  --  thing
  -- ok it works but it replicates on the wrong side...
  -- ok it works.... now can we


search :: Connection -> Params -> IO LT.Text
search conn params = do
  {-
      The main facet search where we take a facet query
      and work out the records and output the concepts and metadata as xml

      -- TODO - control logging in a switch
  -}
  -- let trace_ = False

  ------------------------------------

  -- get the child/parent concept conceptNestings
  conceptNestings <- FacetCalc.getConceptNesting conn
  -- print "# conceptNestings"
  -- mapM print conceptNestings

  -- get initial list records - with records recorded on leaf nodes
  facetList <- FacetCalc.getConceptRecordList conn
  -- print "# facetLeaf counts "
  -- mapM print facetList

  -- turn into a map
  let leafFacetMap = FacetCalc.mapFromList facetList
  -- print "# leafFacetMap after creating the leaf map "
  -- printMap leafFacetMap


  -- propagated records up the concept map
  let facetMap' = FacetCalc.propagate conceptNestings leafFacetMap
  -- print "# propagated facetMap"
  -- printMap facetMap


  -- decode the facet selection criteria - as a single concept
  -- TODO parse full A u B and A ^ B type expressions - easy enough
  let facetTerm = facetQ params
  conceptSelect <- Query.resolveTerm conn facetTerm


  print "------"
  print "WHOOT" 
  print $ Search.any params

  -- uggh - we're going to need a monadic maybe....
 



  -- select the records we are interested in according to the facet criteria
  -- Nothing will select the root node - nice...
  let facetSelectionLst = mapGet conceptSelect facetMap'


  -- ppply freetext search, if no freetext criteria return facetSelectionLst
  freeTextLst <- maybe (return facetSelectionLst) (FreeText.search conn) (Search.any params)

  -- now take intersection of facet and freeText
  let lst = Set.toList $ Set.intersection (Set.fromList freeTextLst) (Set.fromList facetSelectionLst)

  -- FreeText(search)
  -- we need to take the intersection of the record ids.... 

  print "------"
  print lst 
 


  -- create a set for fast inclusion testing of selected records....
  let s = Set.fromList lst

  -- map over the initial leaf map - and prune all records excepted selected
  let facetMap'' = Map.map f leafFacetMap
        where
          f (accum,records) =
            let filteredRecords = filter (\e -> Set.member e s) records in
            (accum, filteredRecords)



  -- propagate records up the leaf map again
  let facetMap = FacetCalc.propagate conceptNestings facetMap''




  -- get the concept, parent and label from db as a Map
  let makePair (concept, parent, label) =
        (Just concept, (parent, label))  -- turn into key,val pairs needed for map,

  labels <- FacetCalc.getConceptLabels conn
      >>= return.(Map.fromList).(map makePair)
  -- print "# labels"
  -- printMap labels


  -- join all label information with the concept/facet map
  let facetMapWithLabels =
       Map.foldlWithKey f [] facetMap
        where
        f m concept records =
          let (parent, label) = mapGet concept labels in
            case concept of
              Nothing ->
                {-
                  review is this still the case...
                  the root node, which appears once - and is'nt a concept or something that we have a label
                  and we cannot store the parent parent_id which will be Nothing since then we get a self-referential child/parent
                  that will create infinite recursion when we go to format the graph.
                -}
                -- trace ("this is the rootNode count " ++ show count ) $
                m
              Just concept_id ->
                -- a normal concept
                -- (concept, parent, label, count) : m
                (concept, parent, label, length records) : m

  -- print "# complete facet list"
  -- (mapM print) facetMapWithLabels

  -- TODO maybe do the sort before joining labels?
  -- rearrange graph for output formatting
  let facetGraph = Summary.fromList facetMapWithLabels
  -- printMap facetGraph

  -- sort the graph - currently by internal id
  let sortedGraph = Summary.sort facetGraph
  -- print "# sorted graph"
  -- printMap facetGraph


  -- select records from the root node,
  let allRecordIds = mapGet Nothing facetMap

  print $ "allRecordIds: " ++ (show.length $ allRecordIds) ++ " " ++  show allRecordIds


  -- generate summary xml
  let summaryXML = Summary.formatXML (length allRecordIds) sortedGraph

  -- handle pagination
  let count = to params - from params + 1
  let pagedIds = take count $ drop (from params - 1) allRecordIds

  print $ "paged ids: " ++ show pagedIds

  -- get record data to return
  records <- RecordGet.getRecords conn pagedIds

  -- generate metadata xml
  let metadataXML = Metadata.formatXML records 1

  -- generate response,
  return $ H.concatLT [
      "<response",
        " from=\"", LT.pack.show.from $ params, "\"",
        " to=\"",   LT.pack.show.to $ params, "\"",
        " selected = \"0\">",
      "\n",
      summaryXML,
      metadataXML,
      "\n",
      "</response>"
    ]


----
-- test


main :: IO ()
main = do
  conn <- PG.connect Config.connectionInfo

  s <- search conn $ Params { from = 0, to = 10000, facetQ = Nothing, Search.any = Nothing }
  LT.putStrLn $ s

  PG.close conn


