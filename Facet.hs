
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
  -- return a flat list of concept nestings
  -- get parent child concept nestings
  -- we want the concept_id, parent_id, record_id
  let query1 = [r|
      select
        id as concept_id,
        parent_id
      from concept_view ;
  |]
  xs :: [ (Integer, Maybe Integer ) ] <- PG.query conn query1 ()
  return xs



getFacetList conn  = do
  -- TODO - will need to be refined, where more
  -- we want the concept_id, parent_id, record_id
  let query1 = [r|
      select
        facet.concept_id,
        concept_view.parent_id,   -- parent concept
        facet.record_id           -- the record
      from facet
      left join concept_view on concept_view.id = facet.concept_id
      order by concept_id
      -- where concept_id = 576 ;
  |]
  -- note the parent may be null! beautiful...
  xs :: [ (Integer, Integer, Integer ) ] <- PG.query conn query1 ()
  -- mapM print xs
  return xs


--

buildFacetMap xs =
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


-- TODO move the nestings argument so it's first - to make it easier to partially bind

propagateRecordsToParentConcept m nestings =
  {-
      a little bit like a topological sort,
      fold over the concept/parent nestings relationships and push the list of record_id's into their parent concept list
      the parent node thus is the union of record_id of it's child nodes
      also remove duplicates

      so we move records up.... and add to the count of records moved up....
  -}

  foldl f Map.empty nestings
  where
    f newMap (concept_id, parent_id) =

      case Map.member (Just concept_id) m of
        True ->

          -- get the record list for this concept
          let (countForConcept, recordsForConcept) = 
                mapGet m (Just concept_id) 
          in
          -- get the records for the parent
          let (countForParent, recordsForParent) = (
                case Map.member parent_id newMap of
                  False -> (0, [])
                  True -> mapGet newMap parent_id
                )
          in
          -- add the recordsForConcept to the recordsForParent and de-duplicate
          let newParentLst  = mkUniq (recordsForParent ++ recordsForConcept)  in

          -- and store new list for the parent concept. countForParent stays the same
          Map.insert parent_id (countForParent, newParentLst) newMap 

          -- and store the empty set for narrower concept, and update the count
          & Map.insert (Just concept_id) (countForConcept + length recordsForConcept, []) 

        False ->
          -- nothing to do - if there were no record matches associated with this concept
          -- we may want to populate with an empty list...
          newMap



printFacetMap m = do
    m
    & Map.toList
    -- TODO we shouldn't need this.
    -- & map (\(concept_id, xs) -> (concept_id, length xs, xs)) -- add length
    & mapM print




{-
--- IMPORTANT
-- there's an issue - in that if the records are not matched by concepts at the same level
-- then the concepts will propagate up and have different values 

-- can we handle this better?  -- so that we propagate up by adding each time....
-- no - because once it's been propagated then we don't want it to be counted again.

-- SO - why not - maintain the list and the count
-- and when we move up... we will be left with the count

-- and this solves the problem.... 
    of getting the counts for all the nodes 
    printing the damn counts
    and knowing the terminating condition.

-}


-- change name to test?
main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  -- print "nestings"
  -- mapM print nestings

  -- get the facet concept and record associations from the db
  facetList <- getFacetList conn
  -- print "facet list"
  -- mapM print $ facetList


  print "######################## 0"
  let m = buildFacetMap facetList
  printFacetMap m


  print "######################## 1"
  let m'  = propagateRecordsToParentConcept m nestings
  printFacetMap m'


  print "######################## 2"
  let m''  = propagateRecordsToParentConcept m' nestings
  printFacetMap m''


  print "######################## 3"
  let m'''  = propagateRecordsToParentConcept m'' nestings
  printFacetMap m'''


  return ()



