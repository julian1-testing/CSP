
{-# LANGUAGE OverloadedStrings #-}

-- import qualified Facet(getConceptNesting, getFacetList, buildFacetMap, propagateToParent, printMap, main) as F
import qualified Facet as F(getConceptNesting,main)

import Database.PostgreSQL.Simple

main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- change to getNestingFromDB
  nestings <- F.getConceptNesting conn
  -- facetList <- F.getFacetList conn


  -- nice!!!
  F.main
