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



isCoreScheme = do
  getChildren
  >>> hasName "rdf:type"
  >>> getAttrValue "rdf:resource"
  >>> isA ((==) "http://www.w3.org/2004/02/skos/core#ConceptScheme")



isCoreConcept = do
  getChildren
  >>> hasName "rdf:type"
  >>> getAttrValue "rdf:resource"
  >>> isA ((==) "http://www.w3.org/2004/02/skos/core#Concept")


isDescription = do
  isElem >>> hasName "rdf:Description"






--------------------------
-- scheme stuff


parseScheme =
  deep (isElem >>> hasName "rdf:Description") >>>
  proc e -> do
    isCoreScheme -< e
    about <- getAttrValue "rdf:about" -< e
    title <- getChildren >>> hasName "dcterms:title" >>> getChildren >>> getText -< e
    -- description, etc. 
    returnA -< (about, title)


storeSchemes conn s = do
    -- parse
    putStrLn $ "doing schemes"
    schemes <- runX (parseXML s  >>> parseScheme)
    let lst = Prelude.map show schemes
    mapM putStrLn lst
    putStrLn $ "  scheme count " ++ (show.length) schemes
    -- store to db
    let store (url,title) = execute conn "insert into scheme(url,title) values (?, ?)" [url, title]
    mapM store schemes



--------------------------
-- concept stuff


parseConcept =
  deep (isElem >>> hasName "rdf:Description") >>>
  proc e -> do
    isCoreConcept -< e
    about <- getAttrValue "rdf:about" -< e
    prefLabel <- getChildren >>> hasName "skos:prefLabel" >>> getChildren >>> getText -< e
    returnA -< (about, prefLabel)


storeConcepts conn s = do
    -- parse
    putStrLn $ "doing concepts"
    concepts <- runX (parseXML s  >>> parseConcept)
    -- let lst = Prelude.map show concepts
    -- mapM putStrLn lst
    putStrLn $ "  concept count " ++ (show.length) concepts
    -- store to db
    let store (url,label) = execute conn "insert into concept(url,label) values (?, ?)" [url, label]
    mapM store concepts



--------------------------
-- narrower

parseNarrower =
  deep isDescription >>>
  proc e -> do
    isCoreConcept -< e
    resource <- getAttrValue "rdf:about" -< e
    narrower <- getChildren >>> isElem >>> hasName "skos:narrower" >>> getAttrValue "rdf:resource" -< e
    returnA -< (resource, narrower)


storeNarrower conn s = do

    let store conn (url,narrower_url) = execute conn "insert into narrower(concept_id, narrower_id) values ((select id from concept where concept.url = ?), (select id from concept where concept.url = ?))" [url, narrower_url]
    let store' = store conn
    -- narrower
    putStrLn $ "doing narrower"
    narrower <- runX (parseXML s >>> parseNarrower)
    -- let lst = Prelude.map show narrower
    -- mapM putStrLn lst
    putStrLn $ "  narrower count " ++ (show.length) narrower
    -- store
    mapM store' narrower



--------------------------
-- narrowMatch 

parseNarrowMatch =
  deep isDescription >>>
  proc e -> do
    isCoreConcept -< e
    resource <- getAttrValue "rdf:about" -< e
    narrowMatch <- getChildren >>> isElem >>> hasName "skos:narrowMatch" >>> getAttrValue "rdf:resource" -< e
    returnA -< (resource, narrowMatch)


storeNarrowMatchs conn s = do

    let store conn (url,narrower_url) = execute conn "insert into narrow_match(concept_id, narrower_id) values ((select id from concept where concept.url = ?), (select id from concept where concept.url = ?))" [url, narrower_url]

    let store' = store conn

    putStrLn $ "doing narrowMatch"
    narrowMatch <- runX (parseXML s >>> parseNarrowMatch)
    -- let lst = Prelude.map show narrowMatch
    -- mapM putStrLn lst
    putStrLn $ "  narrowMatch count " ++ (show.length) narrowMatch
    -- store
    mapM store' narrowMatch




--------------------------
-- scheme membership
--	<skos:inScheme rdf:resource="http://vocab.aodn.org.au/def/discovery_parameter/1"/>
	

parseInScheme =
  deep isDescription >>>
  proc e -> do
    isCoreConcept -< e
    resource <- getAttrValue "rdf:about" -< e
    inScheme <- getChildren >>> isElem >>> hasName "skos:inScheme" >>> getAttrValue "rdf:resource" -< e
    returnA -< (resource, inScheme)


storeInScheme conn s = do
    let store conn (url,inScheme_url) = execute conn "insert into in_scheme(concept_id, scheme_id) values ((select id from concept where concept.url = ?), (select id from scheme where scheme.url = ?))" [url, inScheme_url]
    let store' = store conn
    -- inScheme
    putStrLn $ "doing inScheme"
    inScheme <- runX (parseXML s >>> parseInScheme)
    let lst = Prelude.map show inScheme
    mapM putStrLn lst
    putStrLn $ "  inScheme count " ++ (show.length) inScheme
    -- store
    mapM store' inScheme





--------------------------



-- want to try to pull out the top concept.

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- should we be using plural?
  execute conn "truncate scheme, concept, narrower, narrow_match, in_scheme ;" ()


{-
  param         <- readFile "./vocab/aodn_aodn-discovery-parameter-vocabulary.rdf"   -- 174 prefLabels, no narrower, 1 narrowMatch - prefLabels are detail
  paramCategory <- readFile "./vocab/aodn_aodn-parameter-category-vocabulary.rdf"   -- 32 prefLabels, with 29 narrower, has narrowMatch   - prefLabels are high level

  storeConcepts conn param
  storeConcepts conn paramCategory

  storeNarrowMatchs conn param          -- 1 entry but can't see it
  storeNarrowMatchs conn paramCategory  -- 174 

  storeNarrower conn param
  storeNarrower conn paramCategory
-}

  platform <- readFile "./vocab/aodn_aodn-platform-vocabulary.rdf"              -- 396 prefLabels, with narrower, no narrowMatch - prefLabels are detail 
  platformCategory <- readFile "./vocab/aodn_aodn-platform-category-vocabulary.rdf"     -- 9 preflabels,  no narrower, has 1narrowMatch    - prefLabels are high level

{-
  storeNarrowMatchs conn platform          -- 1 entry but can't see it
  storeNarrowMatchs conn platformCategory -- 174 

  storeNarrower conn platform
  storeNarrower conn platformCategory
-}

  storeSchemes conn  platform
  storeSchemes conn platformCategory

  storeConcepts conn platform
  storeConcepts conn platformCategory

  storeInScheme conn  platform
--  storeInScheme conn platformCategory


-- TODO change storeNarrower to storeNarrowers, 
-- the storeConcepts can also do the scheme membership... 

  close conn
  putStrLn "finished"


