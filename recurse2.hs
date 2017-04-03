
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- {-#  NoMonomorphismRestriction #-}

import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Map as Map

import Control.Arrow ((>>>), (<<<))

-- https://www.reddit.com/r/haskell/comments/4gmw1u/reverse_function_application/
import Data.Function( (&) )

{-
  - remember it's not a tree - and we cannot necessarily easily recurse.

  - instead sweep through as flat lists - and move the items to their parents.  
  - can be a list that we de-duplicate.... or another map

  - until we get to the top node.

  ------------

    select all the facets and records

    select * from facet left join concept_view on concept_view.id = facet.concept_id  where concept_id = 576 ;

    select record_id, concept_id, parent_id from facet left join concept_view on concept_view.id = facet.concept_id  where concept_id = 576 ;

-}


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


-- ease syntax
mapGet = (Map.!)

-- http://stackoverflow.com/questions/4090168/is-there-an-inverse-of-the-haskell-operator
a $> b = b a


buildFacetMap xs =

  Map.empty
  & \m -> foldl f  m xs
  & \m -> foldl f' m xs
  & \m -> foldl f2 m xs

  where
    --  insert an empty list for concept_id
    f m (concept_id, _, _) = 
      Map.insert concept_id [] m

    -- the same - except for parent_id
    f' m (_, parent_id, _) = 
      Map.insert parent_id [] m

    -- populate concept list with the records
    f2 m (concept_id, _, record_id) =
      let current = mapGet m concept_id in
      let new = record_id : current in 
      Map.insert concept_id new m 



propagateFacetMap m xs =

  -- hang on the list that we want to process is from the map?????
  -- we will just iterate for all concepts... o
  -- it's simply a transition from one map to the parent map 

  foldl f m xs

  where
    --  insert an empty list for concept_id
    f m (concept_id, parent_id, _) = 

      let current = mapGet m concept_id in

      -- clear out old map
      let m' = Map.insert concept_id [] m in

      -- insert in parent 
      Map.insert parent_id current m'
      --m'



      -- m -- Map.insert concept_id [ ] m




 
{-
  -- only now we want to propagate everything to the parent 
  -- and perhaps 

  -- issue in propagating up --- is that we may propagate more than once....
  -- depending on the ordering....

  -- holy hell....

  -- is there not a simpler way to do this...
 
  -- IMPORTANT... 
  -- create a new map ...

-}



main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  facetList <- getFacetList conn

  -- mapM print $ facetList
  -- build mapping from concept -> records
  let m = buildFacetMap facetList

  mapM print (Map.toList m) 

  print "########################"

  -- let m' = propagateFacetMap m facetList
  -- mapM print (Map.toList m') 

  propagateFacetMap m facetList & Map.toList & mapM print 
 
  
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

