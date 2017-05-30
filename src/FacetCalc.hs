{-
  Calculate the ConceptRecord graph
-- TODO  consider factoring all the sql actions out of here - similar to what we did with RecordStore and RecordGet.
    - NO - because the test here is self contained.
-- although they are required

-}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-#  NoMonomorphismRestriction #-}

module FacetCalc where


import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import qualified Data.Map as Map

-- https://www.reddit.com/r/haskell/comments/4gmw1u/reverse_function_application/
import Data.Function( (&) )
import Data.Set(toList, fromList)
import Debug.Trace(trace)
import Text.RawString.QQ
-- TODO remove with resolveTerm
import qualified Data.ByteString.Char8 as BS


import qualified Config as Config(connString)

-- deduplicate - O log n
-- http://stackoverflow.com/questions/16108714/haskell-removing-duplicates-from-a-list
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList


-- ease syntax
mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e



getConceptNesting conn = do
  -- TODO maybe change name - get ConceptParents   conceptParents ? ConceptRelationships?
  -- returns a flat list of concept nestings
  let query1 = [r|
      select
        concept_id,
        parent_id
      from concept_view ;
  |]
  xs :: [ (Int, Maybe Int ) ] <- PG.query conn query1 ()
  return xs



getConceptLabels conn = do
  -- same as above - except with the concept labels
  let query1 = [r|
      select
        concept_id,
        parent_id,
        label
      from concept_view ;
  |]
  xs :: [ (Int, Maybe Int, String ) ] <- PG.query conn query1 ()
  return xs



getConceptRecordList conn = do
  -- OK - this thing needs to be changed --- so that we have the damn record
  -- associate concepts with records
  -- we want all concepts - regardless of whether there were facet match counts
  -- shouldn't this be data_parameter ???
  let query1 = [r|
      select
        concept_view.concept_id,
        concept_view.parent_id,
        data_parameter.record_id
      from concept_view
      left join data_parameter on data_parameter.concept_id = concept_view.concept_id
      -- left join record on data_parameter.record_id = record.id
  |]
  xs :: [ (Int, Maybe Int, Maybe Int ) ] <- PG.query conn query1 ()
  -- mapM putStrLn xs
  return xs



-- ok, this restricts the view - to just matching labels...
-- the question is can we structure this - to handle the full nesting...



-- if we select a midlevel term - then we have to pick up all the facets and records below it.
{-
getConceptRecordList2 conn = do
  let query1 = [r|

      select
        concept_view.concept_id,
        concept_view.parent_id,
        data_parameter.record_id
      from concept_view
      left join data_parameter on
        data_parameter.concept_id = concept_view.concept_id
        and concept_view.label = 'mooring'

  |]
  xs :: [ (Int, Maybe Int, Maybe Int ) ] <- PG.query conn query1 ()
  -- mapM putStrLn xs
  return xs
-}



-- change name mapFromList

mapFromList xs =
  -- turn a flat concept/record list into a map of concept -> records, for all the leaf/terminal concepts

  Map.empty
  & \m -> foldl initForConcept m xs
  & \m -> foldl f m xs
  & \m -> Map.insert Nothing ([] :: [ Int ] , []) m    -- insert a root node

  where
    --  insert an empty list for concept_id
    initForConcept m (concept_id, _, _) =
      Map.insert (Just concept_id) ([], []) m

    -- populate concept list with the records
    f m (concept_id, _, record) =
      case record of
        Just record_id ->
          let (accum, current) = mapGet (Just concept_id) m in
          let newLst = record_id : current in
          Map.insert (Just concept_id) (accum, newLst) m
        Nothing ->
          -- nothing means null in the left join so no records
          m



propagateRecordsToParentConcept nestings m =
  {-
      a little bit like a topological sort,
      fold over the concept/parent nestings relationships and push the list of record_id's into their parent concept list
      while incrementing the count of records that move against the child

      we select the records to process first - so to avoid reprocessing things that move more than once in the same pass

      - actually we have to move the records up to the parent. and into the accumulator...
  -}
  let (recordsToProcess, _) = Map.partitionWithKey predHasRecords m in
  foldl ((select recordsToProcess) propagate ) m nestings

  where
    select recordsToProcess f m (concept_id, parent) =
        -- filter for records in recordsToProcess
        case Map.member (Just concept_id) recordsToProcess of
            True -> f m (concept_id, parent)
            False -> m

    propagate m (concept_id, parent) =
        {-
            Using records instead of tuples might maked this cleanermight make this clearer...

        -}
        -- fold over the concept/parent nestings
        -- and update the concept -> records 

        -- get the records associated with child concept
        let (childAccum, childRecords) = mapGet (Just concept_id) m in

        -- get the records for the parent
        let (parentAccum, parentRecords) = mapGet parent m in

        -- update and store for child -  updated Count and an empty list for child...
        Map.insert (Just concept_id) (mkUniq $ childAccum ++ childRecords, []) m
        &
        -- store for parent - parentAccum is unchanged
        Map.insert parent (parentAccum, mkUniq $ parentRecords ++ childRecords)

    predHasRecords k (count, records) =
        not $ null records



propagateAllRecordsToRoot nestings m =
  {-
      keep calling propagateRecordsToParent until all record_ids have been moved to the root node
      maybe we can handle this by clearing of Nothing as wel go
  -}
  case moreToDo m of
    False -> m
    True ->
      propagateRecordsToParentConcept nestings m
      & propagateAllRecordsToRoot nestings
  where
    moreToDo m =
      Map.foldlWithKey f False m

    f m concept (_, records) =
      case concept of
        -- ignore root node
        Nothing -> m
        -- or with more records
        Just _ -> m || ( not $ null records )


putStrLnConceptRecordMap m = do
  -- (mapM $ putStrLn.show).Map.toList $ m
  let l = Map.toList m
  mapM (putStrLn.show) l



flatten m = 
  -- simplify the map of propagated record ids
  Map.mapWithKey f m
  where
    f (Just concept_id) (accum,_) = accum 
    f Nothing (_,children) = children



-- TODO change name
propagate nestings m  =
  propagateAllRecordsToRoot nestings m
  & flatten




testPropagateOnce = do
  -- one nesting level only
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  nestings <- getConceptNesting conn
  facetList <- getConceptRecordList conn
  -- putStrLn "facet list"
  -- mapM putStrLn $ facetList

  let m = mapFromList facetList
  putStrLn "# 0 - leafmap"
  putStrLnConceptRecordMap m

  let m'  = propagateRecordsToParentConcept nestings m
  putStrLn "\n# 1 - after processing one level"
  putStrLnConceptRecordMap m'

{-
  putStrLn "# 2"
  let m''  = propagateRecordsToParentConcept nestings m'
  putStrLnConceptRecordMap m''


  putStrLn "# 3"
  let m'''  = propagateRecordsToParentConcept nestings m''
  putStrLnConceptRecordMap m'''

  putStrLn "# 4"
  let m''''  = propagateRecordsToParentConcept nestings m'''
  putStrLnConceptRecordMap m''''


  putStrLn "# 5"
  let m'''''  = propagateRecordsToParentConcept nestings m''''
  putStrLnConceptRecordMap m'''''
-}
  return ()



testPropagateAll = do
  -- one nesting level only
  conn <- PG.connectPostgreSQL Config.connString

  nestings <- getConceptNesting conn
  -- putStrLn "nestings"
  -- mapM putStrLn nestings

  -- get the facet concept and record associations from the db
  facetList <- getConceptRecordList conn
  -- putStrLn "facet list"
  -- mapM putStrLn $ facetList

  let m = mapFromList facetList
  putStrLn "# leafmap"
  putStrLnConceptRecordMap m

  let m' = propagateAllRecordsToRoot nestings m

  putStrLn "\n# after processing"
  -- putStrLnConceptRecordMap m'
  putStrLnConceptRecordMap $ flatten m' 

  -- get rid of this and just flatten the thing entirely
  --let (m'', records) = adjustRootRecord m'
  --putStrLnConceptRecordMap m''
  return ()


main :: IO ()
main =  do
  testPropagateAll
  --testPropagateOnce




{-
adjustRootRecord m =
  -- set the count of the root node and return the records as a list
  let (_, rootRecords) = mapGet Nothing m in
  let rootCount = length rootRecords  in
  let m' = Map.insert Nothing (rootCount, []) m in
  (m', rootRecords)
-}



{-
  the termination function of having all records propagated - won't work... 
  ahhhhhhh - hang on. 
  
    rather than use a count . why not have another list - that is the accumulated list that passes through.
    then we can use the same  

    then we can do another parse to compute the counts....

    
-}

