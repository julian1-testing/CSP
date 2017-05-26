

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Query where

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))
-- import qualified Data.Map as Map

import Text.RawString.QQ

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as LT(pack, empty, append)


import Data.Function( (&) )


-- TODO move to Utils?

pad :: Int -> a -> [a] -> [a]
pad l x xs = replicate (l - length xs) x ++ xs

padR l x xs = xs ++ replicate (l - length xs) x 



{-
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



{-
  view result looks like this...
  -[ RECORD 42 ]-----------------------------------------------------------------------------------------------------------
  concept_id | 42
  label0     | mooring
  label1     | Mooring and buoy
  label2     | AODN Platform Category Vocabulary
  label3     |
  label4     |

  TODO this stuff doesn't belong here...
-}

{-
resolveTerm conn qualifiedTerm = do
  let query1 = [r|
      SET transform_null_equals TO ON;
      select concept_id 
      from qualified_concept_view 
      where label0 = ? and label1 = ? and label2 = ? and label3 = ? and label4 = ? 
  |]
  -- note, support for ByteString is really nice!
  xs :: [ (Only Int) ] <- PG.query conn query1 (qualifiedTerm :: [ (Maybe BS.ByteString) ] )
  return xs

-}

resolveTerm conn term = do

  -- this isn't quite right... if it's not a term - then we shouldn't be doing anything, 
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
  let facetTerm = Just $ BS.pack "hithere"
  Query.resolveTerm conn facetTerm

