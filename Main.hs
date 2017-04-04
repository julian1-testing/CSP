
{-# LANGUAGE OverloadedStrings #-}

import Facet(getConceptNesting, getFacetList, buildFacetMap, propagateToParent, printMap)

import Database.PostgreSQL.Simple

main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- change to getNestingFromDB
  nestings <- getConceptNesting conn
  facetList <- getFacetList conn


  print "######################## 0"
  let m = buildFacetMap facetList
  printMap m

  print "######################## 1"
  let m'  = propagateToParent m nestings
  printMap m'


  print "######################## 2"
  let m''  = propagateToParent m' nestings
  printMap m''

  print "######################## 3"
  let m'''  = propagateToParent m'' nestings
  printMap m'''

  return ()


