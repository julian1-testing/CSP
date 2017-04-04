


{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Facet where 

-- {-#  NoMonomorphismRestriction #-}

import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Map as Map

import Control.Arrow ((>>>), (<<<))

-- https://www.reddit.com/r/haskell/comments/4gmw1u/reverse_function_application/
import Data.Function( (&) )


import Data.Set(toList, fromList)

import Debug.Trace(trace)


{-
  - remember it's not a tree - and we cannot necessarily easily recurse.
  - instead sweep through as flat lists - and move the items to their parents.
  - can be a list that we de-duplicate.... or another map
  - until we get to the top node.

-}


-- O log n
-- http://stackoverflow.com/questions/16108714/haskell-removing-duplicates-from-a-list
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList


-- ease syntax
mapGet m e =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e



getConceptNesting conn  = do
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



-- doesn't have a null parent_id because - it's only the terms that actually appear in the record

getFacetList conn  = do
  -- we want the concept_id, parent_id, record_id
  let query1 = [r|
      select
        facet.concept_id,
        concept_view.parent_id,  -- parent concept
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



-- http://stackoverflow.com/questions/4090168/is-there-an-inverse-of-the-haskell-operator
-- a $> b = b a


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
  -- a little bit like a topological sort,
  -- fold over the concept/parent nestings relationships and push the list of record_id's into their parent concept list

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
          -- combine them
          let newParentLst  = mkUniq (currentParentLst ++ newSet)  in
          -- and store in terms of the parent_id
          Map.insert parent_id newParentLst newMap

        -- ignore - if no records associated with this concept
        False ->
          newMap



printMap m = do
    m
    & Map.toList
    & map (\(concept_id, xs) -> (concept_id, length xs, xs)) -- add length
    & mapM print



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



  -- build a map from concept_id to list of records
  {-
    let m = foldl f Map.empty xs in
    let m' = foldl f' m xs in
    let m'' = foldl f2 m' xs in
    m''
  -}

  -- (\m -> foldl f2 m' xs ).  (\m -> foldl f' m xs ) . (\m -> foldl f m xs )  Map.empty
{-
  (\m' -> foldl f2 m' xs) $  (\m -> foldl f' m xs) $  (foldl f Map.empty xs)
-}
  -- propagate the records into the next level up...
  -- we don't even need to carry the thing through the recursion
  -- only the one that we might be changing.

  -- TODO make sure they are unique...

  -- VERY IMPORTANT - the XS set will have to be generated from the current list
  -- let xs = Map.
  -- this is not so easy....
  -- let xs = Map.toList m in

  -- ok, it's more complicated - because we need concept_id
  -- ok converting to

  -- HANG on. If we are not using the list....
  -- NO. it may be ok. we just loop through everythign to


{-
  case count == 0 of
    True -> s
    False -> pad' (" " ++ s) (count - 1)


  -- only now we want to propagate everything to the parent
  -- and perhaps

  -- issue in propagating up --- is that we may propagate more than once....
  -- depending on the ordering....

  -- holy hell....

  -- is there not a simpler way to do this...

  -- IMPORTANT...
  -- create a new map ...

-}

{-
      case length  newParentLst of
        0 -> newMap
        _ -> Map.insert parent_id newParentLst newMap
-}


