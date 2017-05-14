{-
  Calculate the ConceptRecord graph
-- TODO  consider factoring the sql actions out of here.
-- although they are required

-}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-#  NoMonomorphismRestriction #-}

module FacetCalc where


import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import qualified Data.Map as Map

-- https://www.reddit.com/r/haskell/comments/4gmw1u/reverse_function_application/
import Data.Function( (&) )
import Data.Set(toList, fromList)
import Debug.Trace(trace)

import Text.RawString.QQ



-- deduplicate - O log n
-- http://stackoverflow.com/questions/16108714/haskell-removing-duplicates-from-a-list
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList


-- ease syntax
mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e



getConceptNesting conn  = do
  -- TODO maybe change name - get ConceptParents   conceptParents ? ConceptRelationships?
  -- returns a flat list of concept nestings
  let query1 = [r|
      select
        concept_id,
        parent_id
      from concept_view ;
  |]
  xs :: [ (Integer, Maybe Integer ) ] <- PG.query conn query1 ()
  return xs



getConceptLabels conn  = do
  -- same as above - except with the concept labels
  let query1 = [r|
      select
        concept_id,
        parent_id,
        label
      from concept_view ;
  |]
  xs :: [ (Integer, Maybe Integer, String ) ] <- PG.query conn query1 ()
  return xs



getConceptRecordList conn  = do
  -- OK - this thing needs to be changed --- so that we have the damn record 
  -- associate concepts with records
  -- we want all concepts - regardless of whether there were facet match counts
  -- shouldn't this be data_parameter ???
  let query1 = [r|
      select
        concept_view.concept_id,
        concept_view.parent_id,
        data_parameter.record_id
      from concept_view
      left join data_parameter on data_parameter.concept_id = concept_view.concept_id
      left join record on data_parameter.record_id = record.id
  |]
  xs :: [ (Integer, Maybe Integer, Maybe Integer ) ] <- PG.query conn query1 ()
  -- mapM putStrLn xs
  return xs



buildInitialConceptMap xs =
  -- create a map of concept -> records, for all the leaf/terminal concepts

  Map.empty
  & \m -> foldl initForConcept m xs
  & \m -> foldl f m xs
  & \m -> Map.insert Nothing (0 , []) m    -- insert a root node

  where
    --  insert an empty list for concept_id
    initForConcept m (concept_id, _, _) =
      Map.insert (Just concept_id) (0, []) m

    -- populate concept list with the records
    f m (concept_id, _, record) =
      case record of
        Just record_id ->
          let (count, current) = mapGet (Just concept_id) m in
          let newLst = record_id : current in
          Map.insert (Just concept_id) (count, newLst) m
        Nothing ->
          -- nothing means null in the left join so no records
          m


propagateRecordsToParentConcept nestings m =
  {-
      a little bit like a topological sort,
      fold over the concept/parent nestings relationships and push the list of record_id's into their parent concept list
      while recorded the count of records moved against the child

      we select the records to process first - so to avoid reprocessing things more than once in the same pass
  -}
  let (recordsToProcess, _) = Map.partitionWithKey predHasRecords m in
  foldl ((select recordsToProcess) propagate ) m nestings

  where

    select recordsToProcess f m (concept_id, parent) =
        -- filter for records in recordsToProcess
        case Map.member (Just concept_id) recordsToProcess of
            True -> f m (concept_id, parent)
            False -> m


    propagate m (concept_id, parent_id) =
        -- fold over the concept/parent nestings
        -- and update the records in the parent. - and record the count against the current child node.

        -- get the records associated with child concept
        let (childCount, childRecords) = mapGet (Just concept_id) m in

        -- get the records for the parent
        let (parentCount, parentRecords) = mapGet parent_id m in

        -- work out updated child count with records for this concept
        let updatedChildCount = childCount + length childRecords in  -- must be the existing count

        -- add child's records to the parent and deduplicate
        let updatedParentRecords = mkUniq ( parentRecords ++ childRecords ) in

        -- and store for child...
        Map.insert (Just concept_id) (updatedChildCount, []) m

        &
        -- store for parent
        Map.insert parent_id (parentCount, updatedParentRecords)


    predHasRecords k (count, records) =
        not $ null records



propagateAllRecordsToRoot nestings m =
  {-
      call propagateRecordsToParent until all record_ids have been moved to the root node
      maybe we can handle this by clearing of Nothing as wel go
  -}

  case countUnprocessed m of   -- change to countUnrpocessed = 0 _ otherwise
    0 -> 
      -- finished
      m
    _ ->
      -- keep processing, more to do
      propagateRecordsToParentConcept nestings m
      & propagateAllRecordsToRoot nestings
  where
    countUnprocessed m =
      Map.foldlWithKey f 0 m

    f m concept_id (_, recordsForConcept) = 
      case concept_id of
        -- ignore root node
        Nothing -> m
        -- sum record count 
        Just _ -> m + length recordsForConcept




adjustRootRecord m = 
  -- set the count of the root node and return the records as a list 
  let (_, rootRecords) = mapGet Nothing m in
  let rootCount = length rootRecords  in
  let m' = Map.insert Nothing (rootCount, []) m in
  (m', rootRecords)



doAll nestings m  =
  propagateAllRecordsToRoot nestings m
  & adjustRootRecord




putStrLnConceptRecordMap m = -- do
  (mapM $ putStrLn.show).Map.toList $ m



testPropagateOnce = do
  -- one nesting level only
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  facetList <- getConceptRecordList conn
  -- putStrLn "facet list"
  -- mapM putStrLn $ facetList

  putStrLn "######################## 0 - leafmap"
  let m = buildInitialConceptMap facetList
  putStrLnConceptRecordMap m

  putStrLn "\n######################## 1 - after processing one level"
  let m'  = propagateRecordsToParentConcept nestings m
  putStrLnConceptRecordMap m'


  putStrLn "######################## 2"
  let m''  = propagateRecordsToParentConcept nestings m'
  putStrLnConceptRecordMap m''


  putStrLn "######################## 3"
  let m'''  = propagateRecordsToParentConcept nestings m''
  putStrLnConceptRecordMap m'''

  putStrLn "######################## 4"
  let m''''  = propagateRecordsToParentConcept nestings m'''
  putStrLnConceptRecordMap m''''


  putStrLn "######################## 5"
  let m'''''  = propagateRecordsToParentConcept nestings m''''
  putStrLnConceptRecordMap m'''''

  return ()



testPropagateAll = do
  -- one nesting level only
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  -- putStrLn "nestings"
  -- mapM putStrLn nestings

  -- get the facet concept and record associations from the db
  facetList <- getConceptRecordList conn
  -- putStrLn "facet list"
  -- mapM putStrLn $ facetList

  let m = buildInitialConceptMap facetList
  --  putStrLnConceptRecordMap m

  let m' =  propagateAllRecordsToRoot nestings m

  let (m'', records) = adjustRootRecord m'

  putStrLnConceptRecordMap m''
  return ()



main :: IO ()
main = testPropagateAll



