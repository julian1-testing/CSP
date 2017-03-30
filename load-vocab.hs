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

{-
http://vocabs.ands.org.au/repository/api/lda/aodn/aodn-discovery-parameter-vocabulary/version-1-2/resource.xml?uri=http://vocab.aodn.org.au/def/discovery_parameter/894

http://vocab.aodn.org.au/def/discovery_parameter/894

Concentration of inferred chlorophyll from relative fluorescence per unit volume of the water body

https://s3-ap-southeast-2.amazonaws.com/content.aodn.org.au/Vocabularies/parameter-category/aodn_aodn-parameter-category-vocabulary.rdf


<rdf:Description rdf:about="http://vocab.aodn.org.au/def/discovery_parameter/894">
	<rdf:type rdf:resource="http://www.w3.org/2000/01/rdf-schema#Resource"/>
	<rdf:type rdf:resource="http://www.w3.org/2004/02/skos/core#Concept"/>
	<dcterms:contributor rdf:datatype="http://www.w3.org/2001/XMLSchema#string">eMII_Mancini.Sebastien</dcterms:contributor>
	<dcterms:created rdf:datatype="http://www.w3.org/2001/XMLSchema#dateTime">2015-11-18T00:00:00Z</dcterms:created>
	<dcterms:creator rdf:datatype="http://www.w3.org/2001/XMLSchema#string">Sebastien Mancini</dcterms:creator>
	<dcterms:modified rdf:datatype="http://www.w3.org/2001/XMLSchema#dateTime">2015-11-19T02:18:19Z</dcterms:modified>
	<dc:publisher rdf:datatype="http://www.w3.org/2001/XMLSchema#string">eMarine Information Infrastructure (eMII)</dc:publisher>
	<dc:source rdf:datatype="http://www.w3.org/2001/XMLSchema#string">Australian Ocean Data Network discovery parameter register</dc:source>
	<skos:broadMatch rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/19"/>
	<skos:inScheme rdf:resource="http://vocab.aodn.org.au/def/discovery_parameter/1"/>
	<skos:prefLabel xml:lang="en">Concentration of inferred chlorophyll from relative fluorescence per unit volume of the water body</skos:prefLabel>
	<skos:topConceptOf rdf:resource="http://vocab.aodn.org.au/def/discovery_parameter/1"/>
</rdf:Description>

-}

parseXML s = readString [ withValidate no
    , withRemoveWS yes  -- throw away formating WS
    ] s


atTag tag = deep (isElem >>> hasName tag)


-- need to load terms, then load relationships.


parseDescription = atTag "rdf:Description" >>>
  proc l -> do
    about <- getAttrValue "rdf:about" -< l

    prefLabel <- getChildren >>> hasName "skos:prefLabel" >>> getChildren >>> getText -< l

    returnA -< (about, prefLabel)



loadVocab conn s = do
 
    dataParameters <- runX (parseXML s  >>> parseDescription)

    putStrLn $  (show. length) dataParameters
 
    let lst = Prelude.map (\term -> show term ) dataParameters
    mapM putStrLn lst




main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- execute conn "truncate terms, relationships;"  ()

  s <- readFile "./vocab/aodn_aodn-discovery-parameter-vocabulary.rdf" 

  -- putStrLn s 
  loadVocab conn s
  putStrLn "  finished"

  

