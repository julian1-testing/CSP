
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-#  NoMonomorphismRestriction #-}

module Facet where


import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Map as Map

-- https://www.reddit.com/r/haskell/comments/4gmw1u/reverse_function_application/
import Data.Function( (&) )
import Data.Set(toList, fromList)
import Debug.Trace(trace)




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
  xs :: [ (Integer, Maybe Integer ) ] <- query conn query1 ()
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
  xs :: [ (Integer, Integer, Integer ) ] <- query conn query1 ()
  -- mapM print xs
  return xs



buildFacetMap xs =

  Map.empty
  & \m -> foldl conceptEmpty  m xs
  -- & \m -> foldl f' m xs
  & \m -> foldl f m xs

  where
    --  insert an empty list for concept_id
    conceptEmpty m (concept_id, _, _) =
      Map.insert (Just concept_id) [] m

    --    -- the same - except for parent_id
    --    f' m (_, parent_id, _) =
    --      Map.insert parent_id [] m
    --      m
    -- TODO - maybe don't populate everything - not sure... - decide later -

    -- populate concept list with the records
    f m (concept_id, _, record_id) =
      let current = mapGet m (Just concept_id) in
      let new = record_id : current in
      Map.insert (Just concept_id) new m



propagateToParent m nestings =
  {-
      a little bit like a topological sort,
      fold over the concept/parent nestings relationships and push the list of record_id's into their parent concept list
      the parent node thus is the union of record_id of it's child nodes
      also remove duplicates
  -}

  foldl f Map.empty nestings
  where
    f newMap (concept_id, parent_id) =
      -- trace  ("trace nestings -> " ++ show (concept_id, parent_id)) $

      case Map.member (Just concept_id) m of
        True ->
          -- trace  ("trace here2 -> " ++ show (concept_id, parent_id)) $

          -- get the set for this concept
          let newSet = mapGet m (Just concept_id) in
          -- get the list of records for the parent - might be top level - Nothing
          let currentParentLst = (
                case Map.member parent_id newMap of
                  False -> []
                  True -> mapGet newMap parent_id
                )
          in
          -- concat the lists
          let newParentLst  = mkUniq (currentParentLst ++ newSet)  in
          -- and store in terms of the parent_id
          Map.insert parent_id newParentLst newMap

        -- nothing to do - if there were no record matches associated with this concept
        -- we may want to populate with an empty list...
        False ->
          newMap



printMap m = do
    m
    & Map.toList
    & map (\(concept_id, xs) -> (concept_id, length xs, xs)) -- add length
    & mapM print



-- OK, now we want to convert from the graph to a tree, with labels. and perhaps depth?
-- so we don't need to maintain in a recursion? not sure.
-- it has to be a map

-- do want to call it until everything has been propagated...


-- change name to test?
main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  -- print "nestings"
  -- mapM print nestings

  -- get the facet concept and record associations from the db
  facetList <- getFacetList conn
  -- print "facet list"
  -- mapM print $ facetList


  print "######################## 0"
  let m = buildFacetMap facetList
  printMap m

  print "######################## 1"
  let m'  = propagateToParent m nestings
  printMap m'

  print "######################## 2"
  let m''  = propagateToParent m' nestings
  printMap m''

  print "######################## 3"
  let m'''  = propagateToParent m'' nestings
  printMap m'''

  return ()



