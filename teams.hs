{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
 
-- This example demonstrates a more complex XML parse,
-- involving multiple levels, attributes, inner lists,
-- and dealing with optional data.
 
-- Example data drawn from:
-- http://www.ibiblio.org/xml/books/bible/examples/05/5-1.xml
-- save as: simple2.xml
 
data Team = Team 
  { teamName, division, league, city :: String,
    players :: [Player] }
  deriving (Show, Eq)
 
data Player = Player
  { firstName, lastName, position :: String,
 
    atBats, hits :: Maybe Int,
    era          :: Maybe Float }
  deriving (Show, Eq)
 
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
    divi     <- atTag "gmd:protocol"  -< l
--    diviName <- getAttrValue "NAME" -< divi
--    team     <- atTag "TEAM"        -< divi
--    teamName <- getAttrValue "NAME" -< team
    returnA -< (divi, divi, divi)


 
main = do
  teams <- runX (parseXML "argo.xml" >>> getTeams1)
  print teams

 
