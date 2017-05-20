{-
  
  Format the result of xml.search.imos, used for Facets

https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes #-}


module FacetFormat where


import qualified Data.Map as Map
import qualified Data.List as List(sortOn, unfoldr)
import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import qualified Data.Text.Lazy as LT(pack, empty, append)
import Data.Function( (&) )
import Debug.Trace(trace)

import Text.RawString.QQ


import qualified FacetCalc as FacetCalc

-- TODO use Option.maybe()
maybeToString m = 
  LT.pack $ case m of
    Just uuid -> uuid
    Nothing -> ""
-}

-- OK. so if we have a set of record_id - we should be able to format them all 






{-
  VERY IMPORTNAT
  IT HAS TO BE A FLAT MAP - because it's a graph not a tree

  mapping from concept_id -> children

  I think we will create another one

  concept_id -> counts

  therefore we should remove the count from here.
-}
{-
    - we need to recurse twice - once to propagate the counts up the tree nodes.
    - this should be non monadic...

-}
{-
-- using multiple db queries to extract the tree is slow - quicker to get everything flat and then destructure to a tree
-- BUT - first - we need to get the counts being returned and then propagating up

-- So it will always require a custom query....
-- also we need to be returning for all vocab not just parameter


-- IMPORTANT
-- are we sure we cannot use a groupby on the facet_count_view to get the counts ...
-- this would make client side stuff a lot simpler.
-}



