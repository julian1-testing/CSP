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
mapGet m e =
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

  where
    --  insert an empty list for concept_id
    initForConcept m (concept_id, _, _) =
      Map.insert (Just concept_id) (0, []) m

    -- populate concept list with the records
    f m (concept_id, _, record) =
      case record of 
        Just record_id -> 
          let (count, current) = mapGet m (Just concept_id) in
          let newLst = record_id : current in
          Map.insert (Just concept_id) (count, newLst) m
        Nothing -> m
          --Map.insert (Just concept_id) (0, []) 

-- it's basically a partition... can we use an existing partition function...


-- i think we have to do it with an idea of whether it's been handled or not... in this pass....

-- if move in seperate pass,
-- no we are just deleting  then coming along later and removing them!!!
-- we can only do the subset that we changed....

-- alternatively if we do it in one pass, then we move items to their parent - then come along and 
-- move their parent up....

-- need to think about this...
-- we are removing from the list so it doesn't get processed again... 
-- but there's nothing to stop moving to a later thing - that then gets re-processed.

-- partition the list - into the things that have members and those that don't.

-- just use a completely different list...
-- yes partition - then we only process the items that need processing
-- get the list of entries... only and generate the new list -
-- but ... ol
-- partition, then process items needing processing. then merge.
-- need to get in one go everything that needs to be pushed. then push only them. 

-- can record - with a bool. or use a separate list.
-- partition into 


propagateRecordsToParentConcept nestings m =
  {-
      a little bit like a topological sort,
      fold over the concept/parent nestings relationships and push the list of record_id's into their parent concept list
      while recorded the count of records moved

      Thus, the parent node is the union of record_id of it's child nodes for each iteration of the propagation 
      Also, we remove duplicates

      -- TODO IMPORTANT - be careful - we don't propagate a nodes out of the root node - so it's no 
      longer accessible. may need to test. and then not move.


      -- propagating things up....

      -- there might be an issue in moving things up - 
      -- rathero

      -- may want partitionWithKey
      -- if we can't do it with partition - then we should be able to do it with a partitionFold

      -- type is ambiguous...
  -}
 
  -- let (a, b) =  Map.partition pred m  in
  let (a, b) =  Map.partitionWithKey pred m  in

  a


  -- & \(m, m2) -> foldl (f2) m nestings 
  -- & \m -> foldl (f3) m nestings 
 
  where
    
    pred k v = 
        let (count, records) = v 
        in case length records of 
          0 -> True
          _ -> False 

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




propagateAllRecordsToRoot nestings m = 
  {-
      call propagateRecordsToParent until all record_ids have been moved to the root node
  -}
  case unpropagatedRecords m == 0 of
    True -> m  
    False -> 
      propagateRecordsToParentConcept nestings m
      & propagateAllRecordsToRoot nestings
  where
    unpropagatedRecords m =
      Map.foldlWithKey f 0 m
        where
        f m concept_id (_, recordsForConcept) = case concept_id of
          -- ignore root node
          Nothing -> m  
          -- else just keep summing
          Just _ -> m + length recordsForConcept



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

  {-
  putStrLn "######################## 2"
  let m''  = propagateRecordsToParentConcept nestings m' 
  putStrLnFacetMap m''
  -}
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
  putStrLnFacetMap m'
  return ()




-- change name to test?
main :: IO ()
main = testPropagateOnce






{-
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  -- putStrLn "nestings"
  -- mapM putStrLn nestings

  -- IMPORTANT - should we move the db code outside of the facet stuff...
  -- if the nestings were ''

  -- get the facet concept and record associations from the db
  facetList <- getFacetList conn
  -- putStrLn "facet list"
  -- mapM putStrLn $ facetList


  putStrLn "######################## 0"
  let m = buildLeafFacetMap facetList
--  putStrLnFacetMap m

  let m' =  propagateAllRecordsToRoot nestings m
  putStrLnFacetMap m'
-}


{-
  propagateAllRecordsToRoot nestings m
  & (return) >>= (\m -> putStrLnFacetMap m)
-}


{-
  putStrLn "######################## 1"
  let m'  = propagateRecordsToParentConcept nestings m
  putStrLnFacetMap m'


  putStrLn "######################## 2"
  let m''  = propagateRecordsToParentConcept nestings m' 
  putStrLnFacetMap m''


  putStrLn "######################## 3"
  let m'''  = propagateRecordsToParentConcept  nestings m''
  putStrLnFacetMap m'''
-}




