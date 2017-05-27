{-
  resolve textual qualified vocab terms, to their concept_id
  by parsing the qualified terms, and looking it up in the db

    eg. Just \"Platform/Satellite/orbiting satellite/NOAA-19\""  -> Just int
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
-- pad :: Int -> a -> [a] -> [a]
-- pad l x xs = replicate (l - length xs) x ++ xs

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

dbGetTerm conn qualifiedTerm = do
  let query = [r|
      SET transform_null_equals TO ON;
      select concept_id
      from qualified_concept_view
      where label0 = ? and label1 = ? and label2 = ? and label3 = ? and label4 = ?
  |]
  -- do db query 
  xs :: [ (Only Int) ] <- PG.query conn query (qualifiedTerm :: [ (Maybe BS.ByteString) ] )

  -- destructure the return rows
  return $ case xs of
    [ Only concept ] -> Just concept
    _ -> Nothing



resolveTerm conn term = do
  -- TODO perhaps don't accept nothing argument here -- eg. if the facet query cannot be parsed as a term then we shouldn't ever get here
  -- actually - perhaps ok - if the thing can't be parsed - we end up returning Nothing.

  -- parse an prepare term ready for db lookup by db query
  let parsedTerm =
        maybe [] (BS.split '/') term   -- split on '/'
        & map f           -- map to concept scheme
        & reverse
        & map Just        -- turn into Maybe
        & padR 5 Nothing  -- right pad columns
        where
          f "Platform" = "AODN Platform Category Vocabulary"
          f x = x

  concept <- dbGetTerm conn parsedTerm

  print $ "resolved concept: "  ++ show concept
  return concept



main = do
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  let facetTerm = Just "Platform/Satellite/orbiting satellite/NOAA-19"
  Query.resolveTerm conn facetTerm



