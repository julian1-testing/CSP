
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}

-- import Text.XML.HXT.Core

{-
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
-}

import Database.PostgreSQL.Simple

import Text.RawString.QQ


-- do we want the id
-- should carry a recursion depth as well 
-- to make it easy to print...
-- think we should pass 

{-
    case length xs of
      1 -> do
        -- store the concept
        let (concept_id, concept_label) : _ = xs
        execute conn [r|
-}
 

pad s count = 
  case count == 0 of 
    True -> s
    False -> pad (" " ++ s) (count - 1)



recurse conn depth (parent_id, label) = do 

  putStrLn $ (pad "" $ depth * 3) ++  "doing id "  ++ show (parent_id, label)

  let query1 = [r|
        select id, label 
        from concept_view  
        where parent_id = ?
  |]

  xs :: [ (Integer, String) ] <- query conn query1 (Only parent_id)

  -- mapM (recurse conn) xs
  -- mapM print  xs

  mapM (\(a,b) -> recurse  conn (depth + 1 :: Integer) (a,b) )  xs

  return ()



main :: IO ()
main = do

  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  let query1 = [r|
        select id, label 
        from concept_view  
        where parent_id is null 
        and scheme_title ~ ?
  |]

  -- let url = "Platform"  :: String
  let url = "Parameter"  :: String

  xs :: [ (Integer, String) ] <- query conn query1 (Only url)

  -- mapM print xs
  -- mapM (\(id,label) -> recurse conn id) xs
  mapM (recurse conn 0) xs

  -- now we want to recurse... and propagate up.... 
  -- hmmmm it's a bit messy to get the counts.

  return ()




