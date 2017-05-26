{-
  the main facetted search
  Maybe change name to xmlSearchImos
-}
-- {-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Search where

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Database.PostgreSQL.Simple.Internal as Internal(Connection)

-- TODO Should be Data.Map.Lazy as Map ?
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as LT(putStrLn)
import qualified Data.Text.Lazy as LT
-- import qualified Data.Utils.List as List(pad)

import Debug.Trace(trace)
import Data.Function( (&) )

import Control.Monad(unless, when)


import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as E(encodeUtf8)



import qualified FacetCalc as FacetCalc --(buildLeafFacetMap,main)
import qualified Summary as Summary(fromList, sort, formatXML)
import qualified RecordGet as RecordGet(getRecords)
import qualified Metadata as Metadata(formatXML)
import qualified Helpers as H(concatLT, pad)


-- TODO move to Utils,
-- data
pad :: Int -> a -> [a] -> [a]
pad l x xs = replicate (l - length xs) x ++ xs

padR l x xs = xs ++ replicate (l - length xs) x 


-- ease syntax
-- change to Map.lookup that returns Maybe - can then specify the error action...
mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e



printMap m =
  (mapM print).(Map.toList) $ m
  -- mapM print $ Map.toList $ m


data Params = Params {

    to :: Int,
    from :: Int,
    facetQ :: Maybe BS.ByteString
} deriving (Show, Eq)


{-
  No we do the search - and select the propagated map.
  then trim
-}


resolveTerm term = do

  -- this isn't quite right... if it's not a term - then we shouldn't be doing anything, 
  let qualifiedFacet = 
        case term of 
          Just text -> BS.split '/' text 
          Nothing -> []

        & map f 

        & map Just        -- turn into Maybe
        & padR 5 Nothing  -- right pad

        where 
          f "Platform" = "AODN Platform Category Vocabulary" 
          f x = x 
          


  print $ "qualified facetQ: " 
    
  print $ qualifiedFacet


  -- this isn't quite right - it should be parsed, and then if there is no
  --  thing 
  -- ok it works but it replicates on the wrong side...
  -- ok it works.... now can we 


request :: Connection -> Params -> IO LT.Text
request conn params = do
  {-
      do a db lookup - and build a facetCountGraph 
        then format it as XML
      also output the metadata

      TODO  - rename functions to Count.
  -}
  -- let trace_ = False

  ------------------------------------
  print $ "facetQ: " ++ (show.facetQ) params

  let facetTerm = facetQ params 

  
  resolveTerm facetTerm

  -- we are going to have to change this to be maybe types...
  -- 



  ------------------------------------
  -- uggh we want to pad it with nulls....

  -- ok the thing works... 

  -- OK - so we use the db to resolve the symbol... we really want this as a function...


  -- TODO - maybe put all the DB actions into another file -- so there's a clear module interface...
  -- TODO - control logging in a switch
  -- change to getNestingFromDB
  -----------------------
  -- get the child/parent concept nestings
  -- is this a fast lookup, should we move this out of the facet code...
  nestings <- FacetCalc.getConceptNesting conn
  -- if trace_ then mapM print nestings else return [ ]
  -- case trace_ of True -> mapM print nestings
  -- when trace_ $ ( mapM print nestings >> return ())

  -- get the initial leaf records
  facetLeafs <- FacetCalc.getConceptRecordList conn
  -- print "# facetLeaf counts "
  -- mapM print facetLeafs

  -- compute facet counts
  let initialFacetMap = FacetCalc.mapFromList facetLeafs
  -- print "# initialFacetMap after creating the leaf map "
  -- printMap initialFacetMap

  -- initial facet map is not propagated....
  -- why do we even use this function....

  -- get the propagated map
  let facetMap' = FacetCalc.propagate nestings initialFacetMap
  -- print "# propagated facetMap"
  -- printMap facetMap


  {-
      TODO: - just introduce a corresponding unflatten function to the flatten function 
                to ease all this translation
  -}
  -- select a particular record....
  -- we should consider whether we use flatten() or not...
  -- LOOKS like it worked...
  let facetMap'' = Map.mapWithKey f facetMap' 
        where
          f concept records = 
            case concept of 
              Just 352 -> ([], records)
              _ -> ([],[])

  let facetMap = FacetCalc.propagate nestings facetMap''

  -- OK - this is a flat map.... -- 
  -- we can either drill down. jk 

  -- get the concept, parent and label from db as a Map
  let makePair (concept, parent, label) =
        (Just concept, (parent, label))  -- turn into key,val pairs needed for map,

  labels <- FacetCalc.getConceptLabels conn
      >>= return.(Map.fromList).(map makePair)
  -- print "# labels"
  -- printMap labels 

  -- now join the label information with the concept/facet map 
  -- and take the record count - TODO facetMap should be passed as an argument
  -- TODO - this is not a record list. - it's a conceptCountList
  let initialFacetMap =
       Map.foldlWithKey f [] facetMap
        where
        f m concept records =
          let (parent, label) = mapGet concept labels in
            case concept of
              Nothing ->
                {-
                  the root node, which appears once - and is'nt a concept or something that we have a label
                  and we cannot store the parent parent_id which will be Nothing since then we get a self-referential child/parent
                  that will create infinite recursion when we go to format the graph.
                -}
                -- trace ("this is the rootNode count " ++ show count ) $
                m
              Just concept_id ->
                -- a normal concept
                -- (concept, parent, label, count) : m
                (concept, parent, label, length records) : m

  -- print "# complete facet list"
  -- (mapM print) initialFacetMap

  {-
    TODO - review this. 
      suspect we might be able to build it directly....

  -}
  -- rearrange graph for output formatting
  let facetGraph = Summary.fromList initialFacetMap
  -- printMap facetGraph


  let sortedGraph = Summary.sort facetGraph
  -- print "# sorted graph"
  -- printMap facetGraph


  -- get the records for the root node,
  let allRecordIds = mapGet Nothing facetMap

  print $ "allRecordIds: " ++ show allRecordIds


  -- generate summary xml
  let summaryXML = Summary.formatXML (length allRecordIds) sortedGraph


  -- do pagination
  let count = to params - from params + 1
  let pagedIds = take count $ drop (from params - 1) allRecordIds

  print $ "paged ids: " ++ show pagedIds

  -- get record data to return
  records <- RecordGet.getRecords conn pagedIds

  -- generate metadata xml
  let metadataXML = Metadata.formatXML records 1

  -- generate response,
  return $ H.concatLT [
      "<response",
        " from=\"", LT.pack.show.from $ params, "\"",
        " to=\"",   LT.pack.show.to $ params, "\"",
        " selected = \"0\">",
      "\n",
      summaryXML,
      metadataXML,
      "\n",
      "</response>"
    ]



-- request :: IO String
main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  s <- request conn $ Params { from = 0, to = 10000, facetQ = Nothing }

  LT.putStrLn $ s

  return ()


