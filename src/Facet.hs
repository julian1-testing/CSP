{-
  # To compile module to an executable,
    ghc  -main-is Facet.main -outputdir tmp Facet.hs  -o ./Facet
-}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-#  NoMonomorphismRestriction #-}

module Facet where


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
  -- TODO change name - get ConceptParents   conceptParents ?
  -- return a flat list of concept nestings
  -- get parent child concept nestings
  -- we want the concept_id, parent_id, record_id
  let query1 = [r|
      select
        concept_id,
        parent_id
      from concept_view ;
  |]
  xs :: [ (Integer, Maybe Integer ) ] <- PG.query conn query1 ()
  return xs



getConceptLabels conn  = do
  let query1 = [r|
      select
        concept_id,
        parent_id,
        label
      from concept_view ;
  |]
  xs :: [ (Integer, Maybe Integer, String ) ] <- PG.query conn query1 ()
  return xs




{-
        facet.concept_id,
        concept_view.parent_id,   -- parent concept
        facet.record_id           -- the record
      from facet
      left join concept_view on concept_view.id = facet.concept_id
      order by concept_id
-}

getFacetList conn  = do
  -- we want all concepts - regardless of whether there were facet match counts
  let query1 = [r|
      select
        concept_view.concept_id,
        concept_view.parent_id,
        facet.record_id
      from concept_view
      left join facet on facet.concept_id = concept_view.concept_id
  |]
  xs :: [ (Integer, Maybe Integer, Maybe Integer ) ] <- PG.query conn query1 ()
  -- mapM putStrLn xs
  return xs



buildLeafFacetMap xs =
  -- TODO change this so we just insert a new - maybe
  -- we make the concept a Maybe type - so that we can handle Nothing as root node later

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


-- we need to know how this is working?
-- it's the finish condition that we need to test....

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



--  let (propagated, records) = Facet.doAll nestings  facetCounts



---- NO just run the propagation 
---- and manually set the root node and take the elements from it...
---- easy peasy.

-- holdy k
-- we need to change this around...


putStrLnFacetMap m = -- do
  (mapM $ putStrLn.show).Map.toList $ m



testPropagateOnce = do
  -- one nesting level only
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  facetList <- getFacetList conn
  -- putStrLn "facet list"
  -- mapM putStrLn $ facetList

  putStrLn "######################## 0 - leafmap"
  let m = buildLeafFacetMap facetList
  putStrLnFacetMap m

  putStrLn "\n######################## 1 - after processing one level"
  let m'  = propagateRecordsToParentConcept nestings m
  putStrLnFacetMap m'


  putStrLn "######################## 2"
  let m''  = propagateRecordsToParentConcept nestings m'
  putStrLnFacetMap m''


  putStrLn "######################## 3"
  let m'''  = propagateRecordsToParentConcept nestings m''
  putStrLnFacetMap m'''

  putStrLn "######################## 4"
  let m''''  = propagateRecordsToParentConcept nestings m'''
  putStrLnFacetMap m''''


  putStrLn "######################## 5"
  let m'''''  = propagateRecordsToParentConcept nestings m''''
  putStrLnFacetMap m'''''

  return ()



testPropagateAll = do
  -- one nesting level only
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  -- putStrLn "nestings"
  -- mapM putStrLn nestings

  -- get the facet concept and record associations from the db
  facetList <- getFacetList conn
  -- putStrLn "facet list"
  -- mapM putStrLn $ facetList

  let m = buildLeafFacetMap facetList
  --  putStrLnFacetMap m

  let m' =  propagateAllRecordsToRoot nestings m

  let (m'', records) = adjustRootRecord m'

  putStrLnFacetMap m''
  return ()




-- change name to test?
main :: IO ()
-- main = testPropagateOnce
main = testPropagateAll







{-
    f2 m (concept_id, parent_id) =
        -- propagate records up to their parent concept, and adjust counts

        -- get the record list for this concept
        let (countForConcept, recordsForConcept) =
              mapGet m (Just concept_id)
        in
        -- get the records for the parent
        let (countForParent, recordsForParent) = (
              case Map.member parent_id m of
                False -> (0, [])
                True -> mapGet m parent_id
              )
        in
        -- concat recordsForConcept to the recordsForParent and de-duplicate
        let newParentLst  = mkUniq (recordsForParent ++ recordsForConcept)  in

        -- and then store against the parent concept. countForParent is unchanged
        Map.insert parent_id (countForParent, newParentLst) m


        & Map.insert (Just concept_id) (countForConcept + length recordsForConcept, []) -- m


    f3 m (concept_id, parent_id) =
        -- get the record list for this concept
        let (countForConcept, recordsForConcept) =
              mapGet m (Just concept_id)
        in

        -- now clear the list for child/narrower concept, and increment count by nymber of records moved to parent
        Map.insert (Just concept_id) (countForConcept + length recordsForConcept, []) m
-}

{-
    -- uggh - we cannot just go and clean the reocrds - as then there is nothing to propagate...
    clearRecords k (count, records) =
        -- clean records but keep the count
        (count, [] :: [ Integer ])
-}

{-

        -- let (count, records) = v
        -- TODO don't compute length - just test if empty or not
        in case length records of
          0 -> False
          _ -> True
-}

