{-
  resolve textual qualified vocab terms, to their concept_id
    eg. Just \"Platform/Satellite/orbiting satellite/NOAA-19\""  -> int
  
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Query where

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))
import qualified Data.ByteString.Char8 as BS
import Data.Function( (&) )
import Text.RawString.QQ

-- TODO move to Utils?
pad :: Int -> a -> [a] -> [a]
pad l x xs = replicate (l - length xs) x ++ xs

padR l x xs = xs ++ replicate (l - length xs) x 


{-
  eg.
  -[ RECORD 42 ]-----------------------------------------------------------------------------------------------------------
  concept_id | 42
  label0     | mooring
  label1     | Mooring and buoy
  label2     | AODN Platform Category Vocabulary
  label3     |
  label4     |
-}

dbResolveTerm conn qualifiedTerm = do
  let query1 = [r|
      SET transform_null_equals TO ON;
      select concept_id 
      from qualified_concept_view 
      where label0 = ? and label1 = ? and label2 = ? and label3 = ? and label4 = ? 
  |]
  xs :: [ (Only Int) ] <- PG.query conn query1 (qualifiedTerm :: [ (Maybe BS.ByteString) ] )
  return xs



resolveTerm conn term = do
  -- we should not be destructuring the Maybe here
  -- eg. it it's not parsed as a term then don't deal with it
  let qualifiedFacet = 
        case term of 
          Just text -> BS.split '/' text 
          Nothing -> []
        & map f 
        & reverse
        & map Just        -- turn into Maybe
        & padR 5 Nothing  -- right pad

        where 
          f "Platform" = "AODN Platform Category Vocabulary" 
          f x = x 
          
  print "qualified facetQ: " 
  print qualifiedFacet

  concept <- dbResolveTerm conn qualifiedFacet

  print "resolved concept: " 
  print concept

  return ()



main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  let facetTerm = Just "Platform/Satellite/orbiting satellite/NOAA-19"
  Query.resolveTerm conn facetTerm

