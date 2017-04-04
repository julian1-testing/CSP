
{-# LANGUAGE OverloadedStrings #-}

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)

import qualified Facet as Facet(getConceptNesting,main)
import qualified FacetFormat as FacetFormat(main)




main :: IO ()
main = do

  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  -- change to getNestingFromDB
  -- nestings <- F.getConceptNesting conn
  -- facetList <- F.getFacetList conn


  -- nice!!!
  Facet.main
  -- FF.main


