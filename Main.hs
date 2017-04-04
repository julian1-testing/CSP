
{-# LANGUAGE OverloadedStrings #-}

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)

import qualified Facet as Facet--(buildLeafFacetMap,main)
import qualified FacetFormat as FacetFormat--(main)

import qualified Data.Map as Map


import Data.Function( (&) )

mapGet m e = (Map.!) m e




main :: IO ()
main = do

  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  -- change to getNestingFromDB


  -----------------------
  -- get stuff for calculating facet counts
  nestings <- Facet.getConceptNesting conn -- is this a fast lookup, should we move this out of the facet code...
  facetList <- Facet.getFacetList conn

  let m = Facet.buildLeafFacetMap facetList
        & Facet.propagateAllRecordsToRoot nestings 

  -- print "################## nestings list"
  -- mapM print nestings

  print "################## labels list "

  -- TODO change name nestings to parents, or parentNestings or something? or broader ?



  labels <- Facet.getConceptLabels conn 
  mapM print labels


  let labels' =  Map.fromList . map (\(k, a, b) -> (k, (a,b))) $   labels

  -- let labels' = (Facet.getConceptLabels conn) >>=  Map.fromList . map (\(k, a, b) -> (k, (a,b)))  
-- labels


  print "################## labels map"
  mapM print . Map.toList $ labels' 


  print "##################"
  -----------------------

{-
  -- actually we should transform it into the subset of whatever is the most convent

          id as concept_id, 
          parent_id,
          label ,
          -999 -- count  -- dummy value
-} 

  -- if we are going to desctructure this...  we'll have to match...

  -- let nestingsMap = Map.fromList nestings
  -- (Map.toList nestingsMap) & mapM print  

  -- WE DON"T WANT map - because we don't want to have to deal with the parent node... for printing... 
  -- or do we... 
  -- no i 

  -- map means we can't drop the element...

  -- So just do a fold - to reorganise it again... into a list without the parent....
  -- GOOOD

  -- map will preserve the number of elements which isn't much good.
  -- here we add in the parent_id

  -- it has to be a fold or filter to remove the Nothing....

{-   
  let m' = Map.mapWithKey f m 
        where
        f concept (count, _) = case concept of
          Nothing ->         (count,  Nothing,  1234)
          Just concept_id -> (count, mapGet nestingsMap concept_id,  1234)

  -- so we just need to tack in the parent and the label... again...
-}
  -- IMPORTANT - should we move the db code outside of the facet stuff...
  -- if the nestings were ''



  let m' = 
       Map.foldlWithKey f Map.empty m
        where
        f m k (count, records) = case k of 
          -- drop the root node
          Nothing -> m
          -- otherwise just insert 
          Just concept_id -> Map.insert concept_id count m


  -- this
  -- Facet.printFacetMap m'

  (Map.toList m') & mapM print  


 
  return () 

  -- nice!!!
  -- FacetFormat.main




  -- mapM print nestings 
  -- it might easier just to take the Facet structure - and add in the parent_id, and labels?
  -- it's also already a map....

{-
  facetList_ <- FacetFormat.getFacetList conn

  -- this is a bit messy
  -- let  facetList' = map (\f (a,b,c, d) -> (a,b,c, 123) )  facetList

  let g = FacetFormat.buildFacetGraph facetList_

  FacetFormat.printXMLFacetGraph g
-}



{-
  VERY IMPORTANT - we must build the list out of our returned values. 
    because that is the result of our query
      - we just need to add in the nestings 
      - and the labels.

  - and it's already in a nice list...
  - except we have to do it from a list

  - actually it's not quite as simple

  -- it's just a freaking join...
  -- but I think we want to use the actual counts...
-}

  -- but we have to structure it in terms of 
  -- it's already a fast lookup - but we just don't have the children....

  -- I think the only thing we need is to add in the parent and label and we're done.

