{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core

-- parses xml -> to data structure
 
-- This example demonstrates a more complex XML parse,
-- involving multiple levels, attributes, inner lists,
-- and dealing with optional data.
 
-- Example data drawn from:
-- http://www.ibiblio.org/xml/books/bible/examples/05/5-1.xml
-- save as: simple2.xml
 
--data Team = Team 
--  { teamName, division, league, city :: String  }
--  deriving (Show, Eq)
 

--- should use a data structure .... rather than a tuple... 

 
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
 
atTag tag = deep (isElem >>> hasName tag)
 
-- Incremental development of the getTeams function:
 
-- First, list the teams.
-- Try it out in GHCi: 
-- Main> runX (parseXML "simple2.xml" >>> getTeams1)


 
getTeams1 = atTag "gmd:CI_OnlineResource" >>>
  proc l -> do
    -- leagName <- getAttrValue "NAME"   -< l
    protocol <- atTag "gmd:protocol" >>> getChildren >>> hasName "gco:CharacterString" >>> getChildren >>> getText -< l
    url      <- atTag "gmd:linkage"  >>> getChildren >>> hasName "gmd:URL" >>> getChildren >>> getText -< l


-- important see how this is aparently extracting from sub- references.
--    diviName <- getAttrValue "NAME" -< divi
--    team     <- atTag "TEAM"        -< divi
--    teamName <- getAttrValue "NAME" -< team
    returnA -< (protocol, url)


 
main = do
  teams <- runX (parseXML "argo.xml" >>> getTeams1)
  let lst = map (\(a,b) -> " ->" ++ a ++ " ->" ++ b ) teams
  mapM print lst

 
