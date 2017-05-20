{-
  
  Format the summary part of xml.search.imos

https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes #-}


module Summary where


import qualified Data.Map as Map
import qualified Data.List as List(sortOn)
import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import qualified Data.Text.Lazy as LT(pack, empty, append)
import Data.Function( (&) )
import Debug.Trace(trace)

import Text.RawString.QQ

-- import qualified Text.XML.HXT.DOM.Util as XML(attrEscapeXML)
-- import qualified Text.XML.HXT.DOM.Util as XML(stringTrim, escapeUri )
-- import qualified Text.XML.HXT.DOM.Util as X(escapeUri )
-- import qualified Text.XML.HXT.DOM.Util as X(stringTrim)

-- https://www.stackage.org/haddock/lts-7.21/hxt-9.3.1.16/Text-XML-HXT-DOM-Util.html
import qualified Text.XML.HXT.DOM.Util as X(attrEscapeXml)


import qualified FacetCalc as FacetCalc
import qualified Helpers as H(concatLT, pad)


-- ease syntax
mapGet e m =
  -- trace  ("mytrace - mapGet e: " ++ show e ++ " m: " ++ show m) $
  (Map.!) m e




fromList xs =
  -- Map of concept_id -> [ child concepts ]
  -- store the concept/child relationships in a map to enable O(log n) lookup of a nodes children

  Map.empty
  & \m -> Map.insert Nothing [] m    -- add a root/parent node
  & \m -> foldl createEmptyList m xs
  & \m -> foldl insertToList m xs

  where
    -- insert empty list for concept_id
    -- this is ok.
    createEmptyList m (concept_id,_,_,_) =
      Map.insert (concept_id) [] m

    -- add items to the list associated with graph node with children
    insertToList m (concept_id, parent, label,count) =

        let childLst = mapGet parent m in
        let newChildren = (concept_id, label, count) : childLst in
        Map.insert parent newChildren m



sort m =
  -- TODO move to ConceptRecord?
  -- sort the children according to their count
  -- should sort before adding labels to tuples?
  Map.mapWithKey f m
  where
  f k children =
    reverse. List.sortOn (\(_, _, count) -> count) $ children


-- so all we need to do is pass the actual root node in here explicitly 
-- VERY IMPORTANT - it might be possible to do this more simply - by having separate lists. 

-- concatLT lst = concatMap LT.append lst


-- TODO change format to format

formatXML rootRecordCount m = 
  {-  we will recurse from the root node down...
      remember that we cannot have a Nothing node above the Nothing node. so there's nowhere else to store label or count 
      information which isn't a known concept or scheme anyway
  -}
  let rootNode = (Nothing, "summary", rootRecordCount ) in
  recurse m rootNode 0
  where
    -- drill into child nodes
    recurse m (parent, label, count) depth = 

      -- what's going on here?
      case label of 
        "summary" ->
          formatSummary (parent, label, count) depth 

        -- should we be formating this label substitution here? or when loading the vocab scheme and set the label? ...
        "AODN Parameter Category Vocabulary" ->
          formatDimension (parent, "Measured Parameter", count) depth 

        "AODN Platform Category Vocabulary" ->
          formatDimension (parent, "Platform", count) depth 

        _ ->
          formatCategory (parent, label, count) depth 

    formatSummary (parent, label, count) depth  = 
      H.concatLT [
          H.pad $ depth * 3,
          "<", LT.pack label, " count=\"", LT.pack.show $ count, "\" type=\"local\">\n",
          formatChildren (parent, label, count) depth,
          H.pad $ depth * 3,
          "</", LT.pack label, ">"
      ]


    formatDimension (parent, label, count) depth =
      -- single closed tag...
      H.concatLT [
          H.pad $ depth * 3,
          "<dimension value=\"", LT.pack label, "\" count=\"", LT.pack.show $ count, "\">\n", 
          formatChildren (parent, label, count) depth,
          H.pad $ depth * 3,
          "</dimension>"
      ]


    formatCategory (parent, label, count) depth =

      let value = LT.pack $ X.attrEscapeXml label in
      H.concatLT [ 
        -- start tag
        H.pad $ depth * 3,
        -- here
        "<category value=\"", value, "\" count=\"", LT.pack.show $ count, "\">\n",
        -- children
        formatChildren (parent, label, count) depth ,
        -- end tag
        H.pad $ depth * 3,
        "</category>\n"
      ]


    formatChildren (parent, label, count) depth =
        -- take children
        let children = mapGet parent m  in
        -- recurse into children
        let f acc (concept, label, count) = acc $ recurse m (concept, label, count) (depth + 1) in
        -- fold over children appending text
        foldl (f.LT.append) LT.empty children
        -- TODO use concatLT? eg.
        -- concatMap f children



getTestConceptRecordList conn = do
  -- get all facets and facet count from db and return as flat list
  let query = [r|
        select
          concept_id,
          parent_id,
          label ,
          count
        from facet_view
  |]
  xs :: [ (Integer, Maybe Integer, String, Integer) ] <- PG.query conn query ()
  return xs



{-
  Main test/example is in Search
-}


