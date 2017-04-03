
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Database.PostgreSQL.Simple
import Text.RawString.QQ
import qualified Data.Map as Map


{-
  - remember it's not a tree - and we cannot necessarily easily recurse.

  - instead sweep through as flat lists - and move the items to their parents.  

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

-- now we want to condense this into a list -> list ....
-- it does need to be a map so that we can insert into it easily


--   let e = Map.insert Nothing [] e' in  


buildFacetMap xs =

  -- let m = Map.empty in 

  let f m (concept_id, parent_id, record_id) = Map.insert concept_id [ record_id ] m in 
  
  foldl f Map.empty xs
 
{-
  foldl insertToList e xs

  let e' = foldl emptyList Map.empty xs in
  let e = Map.insert Nothing [] e' in  
  foldl insertToList e xs
  where
    -- insert empty list for concept_id
-}



main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  facetList <- getFacetList conn

  mapM print $ facetList

  let m = buildFacetMap facetList

  
  -- mapM print $ facetList
  print m 

  return ()


