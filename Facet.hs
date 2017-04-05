
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
  -- mapM print xs
  return xs



buildLeafFacetMap xs =
  -- TODO change this so we just insert a new - maybe 

  Map.empty
  & \m -> foldl initForConcept m xs
  & \m -> foldl f m xs

  where
    --  insert an empty list for concept_id
    initForConcept m (concept_id, _, _) =
      Map.insert (Just concept_id) (0, []) m

    -- populate concept list with the records
    f m (concept_id, _, record_id) =
      let (count, current) = mapGet m (Just concept_id) in
      let newLst = record_id : current in
      Map.insert (Just concept_id) (count, newLst) m



propagateRecordsToParentConcept nestings m' =
  {-
      a little bit like a topological sort,
      fold over the concept/parent nestings relationships and push the list of record_id's into their parent concept list
      while recorded the count of records moved

      Thus, the parent node is the union of record_id of it's child nodes for each iteration of the propagation 
      Also, we remove duplicates

      -- TODO IMPORTANT - be careful - we don't propagate a nodes out of the root node - so it's no 
      longer accessible. may need to test. and then not move.

      -- why do we have empty ? 

      -- propagating things up....
  -}

  foldl (pred f2) m' nestings 
{-
  m'
  & \m -> foldl (pred f2) m nestings 
  & \m -> foldl (pred f3) m nestings
-} 
  where

    pred ff m (concept_id, parent_id) =
      -- only process concepts that exist in the map
      case Map.member (Just concept_id) m of
        True -> ff m (concept_id, parent_id)
        False -> trace ("whoot " ++ show concept_id) $ m

    -- we have to do this in two steps - so that the same values get propagating into multiple parents
    -- if they exist. before clearing out...

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

        -- now clear the list for child/narrower concept, and increment count by nymber of records moved to parent
        & Map.insert (Just concept_id) (countForConcept + length recordsForConcept, []) -- m



    f3 m (concept_id, parent_id) =
        -- get the record list for this concept
        let (countForConcept, recordsForConcept) = 
              mapGet m (Just concept_id) 
        in

        -- now clear the list for child/narrower concept, and increment count by nymber of records moved to parent
        Map.insert (Just concept_id) (countForConcept + length recordsForConcept, []) m




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



-- remove  or rename to just printMap
printFacetMap = do
  -- m & Map.toList & mapM print
  (mapM print).Map.toList




{-
  now we want to do this until 

-}


-- change name to test?
main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  -- print "nestings"
  -- mapM print nestings

  -- IMPORTANT - should we move the db code outside of the facet stuff...
  -- if the nestings were ''

  -- get the facet concept and record associations from the db
  facetList <- getFacetList conn
  -- print "facet list"
  -- mapM print $ facetList


  print "######################## 0"
  let m = buildLeafFacetMap facetList
--  printFacetMap m

  let m' =  propagateAllRecordsToRoot nestings m
  printFacetMap m'


{-
  propagateAllRecordsToRoot nestings m
  & (return) >>= (\m -> printFacetMap m)
-}


{-
  print "######################## 1"
  let m'  = propagateRecordsToParentConcept nestings m
  printFacetMap m'


  print "######################## 2"
  let m''  = propagateRecordsToParentConcept nestings m' 
  printFacetMap m''


  print "######################## 3"
  let m'''  = propagateRecordsToParentConcept  nestings m''
  printFacetMap m'''
-}

  return ()



