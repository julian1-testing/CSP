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


<rdf:Description rdf:about="http://vocab.aodn.org.au/def/parameter_classes/category/53">
	<rdf:type rdf:resource="http://www.w3.org/2000/01/rdf-schema#Resource"/>
	<rdf:type rdf:resource="http://www.w3.org/2004/02/skos/core#Concept"/>
	<dcterms:created rdf:datatype="http://www.w3.org/2001/XMLSchema#dateTime">2014-06-01T00:00:00Z</dcterms:created>
	<dcterms:creator xml:lang="en">Sebastien Mancini</dcterms:creator>
	<dc:publisher rdf:datatype="http://www.w3.org/2001/XMLSchema#string">eMarine Information Infrastructure (eMII)</dc:publisher>
	<dc:source rdf:datatype="http://www.w3.org/2001/XMLSchema#string">Australian Ocean Data Network parameter category register</dc:source>
	<skos:definition xml:lang="en">This category contains vocabulary terms describing physical atmosphere parameters</skos:definition>
	<skos:inScheme rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/1"/>
	<skos:prefLabel xml:lang="en">Physical-Atmosphere</skos:prefLabel>
	<skos:narrower rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/10"/>
	<skos:narrower rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/22"/>
	<skos:narrower rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/30"/>
	<skos:narrower rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/48"/>
	<skos:narrower rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/6"/>
	<skos:narrower rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/7"/>
	<skos:narrower rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/8"/>
	<skos:narrower rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/category/9"/>
	<skos:topConceptOf rdf:resource="http://vocab.aodn.org.au/def/parameter_classes/1"/>
</rdf:Description>


-}

parseXML s = readString [ withValidate no
    , withRemoveWS yes  -- throw away formating WS
    ] s





parseNarrower =
  deep (isElem >>> hasName "skos:narrower") >>> 
  proc e -> do
    narrower <- getChildren -< e
    returnA -< (narrower)


-- parseNarrower2 =
--  -- deep (isElem >>> hasName "skos:narrower") >>> 
--  deep (isElem >>> hasName "skos:narrower" >>> getAttrValue "rdf:resource"  )
-- so we have a problem that we have 
---- or maybe we don't need it????
---- and we can just write clever sql...



parseConcept = 
  deep (isElem >>> hasName "rdf:Description") >>> 
  proc e -> do
    -- only core#Concept
    getChildren 
        >>> hasName "rdf:type" 
        >>> getAttrValue "rdf:resource" 
        >>> isA ((==) "http://www.w3.org/2004/02/skos/core#Concept") -< e
    
    -- this stuff gets short-circuited if doesn't exist 
    resource <- getAttrValue "rdf:about" -< e
    prefLabel <- getChildren >>> hasName "skos:prefLabel" >>> getChildren >>> getText -< e
    narrowerResource <- getChildren >>> isElem >>> hasName "skos:narrower" >>> getAttrValue "rdf:resource"  -< e

    returnA -< (resource, narrowerResource)


loadConcepts conn s = do
    -- parse 
    dataParameters <- runX (parseXML s >>> parseConcept)

    -- print 
    let lst = Prelude.map show dataParameters
    mapM putStrLn lst

    putStrLn $ "count " ++ (show. length) dataParameters

    -- store to db
    let storeToDB (url,label) = execute conn "insert into concept(url,label) values (?, ?)" [url, label]
    mapM storeToDB dataParameters



main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- should we be using plural?
  execute conn "truncate concept;" ()

  s <- readFile "./vocab/aodn_aodn-parameter-category-vocabulary.rdf" 

  -- putStrLn s 
  loadConcepts conn s
  close conn
  putStrLn "  finished"

  
