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


isCoreConcept = do
  getChildren
  >>> hasName "rdf:type"
  >>> getAttrValue "rdf:resource"
  >>> isA ((==) "http://www.w3.org/2004/02/skos/core#Concept")


isDescription = do
  isElem >>> hasName "rdf:Description"

--------------------------
-- scheme stuff

parseNarrower =
  deep isDescription >>>
  proc e -> do
    isCoreConcept -< e
    resource <- getAttrValue "rdf:about" -< e
    narrower <- getChildren >>> isElem >>> hasName "skos:narrower" >>> getAttrValue "rdf:resource" -< e
    returnA -< (resource, narrower)


parseNarrowMatch =
  deep isDescription >>>
  proc e -> do
    isCoreConcept -< e
    resource <- getAttrValue "rdf:about" -< e
    narrowMatch <- getChildren >>> isElem >>> hasName "skos:narrowMatch" >>> getAttrValue "rdf:resource" -< e
    returnA -< (resource, narrowMatch)


parseCategories =
  deep isDescription >>>
  proc e -> do
    isCoreConcept -< e
    resource <- getAttrValue "rdf:about" -< e
    label    <- getChildren >>> hasName "skos:prefLabel" >>> getChildren >>> getText -< e
    returnA -< (resource, label)


storeConcept conn (url,label) =
  execute conn "insert into concept(url,label) values (?, ?)" [url, label]


storeNarrowerRel conn (url,narrower_url) =
  execute conn "insert into narrower(concept_id, narrower_id) values ((select id from concept where concept.url = ?), (select id from concept where concept.url = ?))" [url, narrower_url]


storeNarrower conn s = do
    let storeConcept' = storeConcept conn

    let storeNarrowerRel' = storeNarrowerRel conn

    -- categories

    putStrLn $ "doing categories"
    categories <- runX (parseXML s >>> parseCategories)
    let lst = Prelude.map show categories
    mapM putStrLn lst
    putStrLn $ "categories count " ++ (show.length) categories

    -- store categories as concepts to db
    mapM storeConcept' categories

    -- narrower
    putStrLn $ "doing narrower"
    narrower <- runX (parseXML s >>> parseNarrower)
    let lst = Prelude.map show narrower
    mapM putStrLn lst
    putStrLn $ "narrower count " ++ (show.length) narrower

    -- store narrower relationships
    mapM storeNarrowerRel' narrower

    -- narrowerMatchmatch
{-
    putStrLn $ "doing narrowMatch"
    narrowerMatch <- runX (parseXML s >>> parseNarrowMatch)
    let lst = Prelude.map show narrowerMatch
    mapM putStrLn lst
    putStrLn $ "narrowMatch count " ++ (show.length) narrowerMatch

    -- store narrowerMatch match
    mapM storeNarrowerRel' narrowerMatch

-}


-- this is kind of harder to work with

--------------------------
-- concept stuff

storeNarrowMatch conn (url,narrower_url) =
  execute conn "insert into narrow_match(concept_id, narrower_id) values ((select id from concept where concept.url = ?), (select id from concept where concept.url = ?))" [url, narrower_url]



parseConcept =
  deep (isElem >>> hasName "rdf:Description") >>>
  proc e -> do
    isCoreConcept -< e
    -- has prefLabel
    about <- getAttrValue "rdf:about" -< e
    prefLabel <- getChildren >>> hasName "skos:prefLabel" >>> getChildren >>> getText -< e
    returnA -< (about, prefLabel)


storeConcepts conn s = do
    -- parse

    putStrLn $ "doing concepts"
    concepts <- runX (parseXML s  >>> parseConcept)
    -- print
    let lst = Prelude.map show concepts
    mapM putStrLn lst
    putStrLn $ "count " ++ (show.length) concepts
    -- store to db
    let storeToDB (url,label) = execute conn "insert into concept(url,label) values (?, ?)" [url, label]
    mapM storeToDB concepts


    let storeNarrowMatch' = storeNarrowMatch conn

    putStrLn $ "doing narrowMatch"
    narrowMatch <- runX (parseXML s >>> parseNarrowMatch)
    let lst = Prelude.map show narrowMatch
    mapM putStrLn lst
    putStrLn $ "narrowMatch count " ++ (show.length) narrowMatch

    -- store narrowerMatch match
    mapM storeNarrowMatch' narrowMatch



--------------------------

-- think we want to take anything with a prefLabel

-- We need to separate out the parsing...

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"

  -- should we be using plural?
  execute conn "truncate concept, narrower, narrow_match ;" ()
{-
  -- parameter
  s <- readFile "./vocab/aodn_aodn-discovery-parameter-vocabulary.rdf"
  storeConcepts conn s

  s <- readFile "./vocab/aodn_aodn-parameter-category-vocabulary.rdf"
  storeNarrower conn s
-}
  -- platform
--  s <- readFile "./vocab/aodn_aodn-platform-vocabulary.rdf"
--  storeConcepts conn s

  -- TODO the parsing should be done once only...




  s <- readFile "./vocab/aodn_aodn-discovery-parameter-vocabulary.rdf"   -- 174 prefLabels, no narrower, 1 narrowMatch - prefLabels are detail
  storeNarrower conn s

  s <- readFile "./vocab/aodn_aodn-parameter-category-vocabulary.rdf"   -- 32 prefLabels, with 29 narrower, has narrowMatch   - prefLabels are high level
  storeConcepts conn s




-- 1. parse all the pref labels out from both files.
-- then fill in the narrower an narrowMatch
-- why not just parse everything to get the pref labels....  first.... and store them...


{-
  s <- readFile "./vocab/aodn_aodn-platform-vocabulary.rdf"              -- 396 prefLabels, with narrower, no narrowMatch - prefLabels are detail 
  storeNarrower conn s

  s <- readFile "./vocab/aodn_aodn-platform-category-vocabulary.rdf"     -- 9 preflabels,  no narrower, has narrowMatch    - prefLabels are high level
  storeConcepts conn s
-}

  -- this looks reasonably correct
  -- psql -h postgres.localnet -U harvest -c 'select * from concept left join scheme on concept.id = scheme.narrower_id '


  close conn
  putStrLn "  finished"


