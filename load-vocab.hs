-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- needed for disambiguating types,
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}


import Text.XML.HXT.Core


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC


import Database.PostgreSQL.Simple




parseXML s = readString [ withValidate no
    , withRemoveWS yes  -- throw away formating WS
    ] s


atTag tag = deep (isElem >>> hasName tag)






parseDataParameters = atTag "rdf:Description" >>>
  proc l -> do
    about <- getAttrValue "rdf:about" -< l

    returnA -< about



loadVocab conn s = do
 
    putStrLn "###############"
    putStrLn "parsing the parameters"

    -- parse data parameters,
    dataParameters <- runX (parseXML s  >>> parseDataParameters)

    putStrLn $  (show. length) dataParameters
 
    let lst = Prelude.map (\term -> show term ) dataParameters
    mapM putStrLn lst




main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  execute conn "truncate catalog, resource;"  ()

  s <- readFile "./vocab/aodn_aodn-parameter-category-vocabulary.rdf" 

  -- putStrLn s 
  loadVocab conn s
  putStrLn "  finished"

  

