
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
  -- get all facets and facet count from db and return as flat list
  let query1 = [r|

          select 
            concept_id, 
            parent_id,
            record_id 
          from facet 
          left join concept_view on concept_view.id = facet.concept_id  
    
          order by concept_id
          -- where concept_id = 576 ;

  |]
  -- note the parent may be null! beautiful...
  xs :: [ (Integer, Integer, Integer ) ] <- query conn query1 ()
  -- mapM print xs
  return xs

-- we want the concept_id, parent_id, record_id 

main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  facetList <- getFacetList conn

  mapM print $ facetList

  return ()


