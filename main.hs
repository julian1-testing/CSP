
-- good doc -> https://wiki.haskell.org/HXT#Installation

module Main
where
 
import Text.XML.HXT.Core
-- import Text.XML.HXT....   -- further HXT packages
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
 
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit



 
main :: IO ()
main
    = do
      print "hi"
      argv <- getArgs
      (al, src, dst) <- cmdlineOpts argv
      [rc]  <- runX (application al src dst)
      if rc >= c_err
      then exitWith (ExitFailure (0-1))
      else exitWith ExitSuccess
     
-- | the dummy for the boring stuff of option evaluation,
-- usually done with 'System.Console.GetOpt'
 
cmdlineOpts   :: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv
    = return ([withValidate no], argv!!0, argv!!1)
 
-- | the main arrow
 
application :: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst
    = configSysVars cfg                                    -- (0)
      >>>
      readDocument [] src
      >>>
      processChildren (processDocumentRootElement)        --`when` isElem)  -- (1)
      >>>
      writeDocument [] dst                                 -- (3)
      >>>
      getErrStatus
 
 
-- | the dummy for the real processing: the identity filter

-- Very Important IOSArrow is probably a function type -eg. with =>
-- IOSArrow is 
 
processDocumentRootElement  :: IOSArrow XmlTree XmlTree
processDocumentRootElement 
    -- =  this -- substitute this by the real application
    =  selectAllText 

-- VERY IMPORTANT - we are not converting to text. We are selecting/walking xml nodes.

-- getText should work. through.
-- but it probably returns a string....


-- this works. 
--  >>> getText >>> mkText 

isOnlineResource :: ArrowXml a => a XmlTree XmlTree
isOnlineResource = isElem >>> hasName "gmd:CI_OnlineResource"

-- works
getProtocol :: ArrowXml a => a XmlTree XmlTree
getProtocol = 
  getChildren >>> hasName "gmd:protocol" >>> 
  getChildren >>> hasName "gco:CharacterString" >>> 
  getChildren >>> isText 

-- Works
getUrl :: ArrowXml a => a XmlTree XmlTree
getUrl = getChildren >>> hasName "gmd:linkage" >>> 
  getChildren >>> hasName "gmd:URL" >>> 
  getChildren 
  >>> getText >>> mkText 
  -- >>> (arr "whoot") 

-- how do we lift a string node ... 

addBrackets :: String -> String
addBrackets s
  =  " [[ " ++ s ++ " ]] "

-- now how would we build a tuple of these values...  
-- and do it for each one.


--              <gmd:linkage>
--                <gmd:URL>http://geoserver-123.aodn.org.au/geoserver/ows</gmd:URL>
 
 

selectAllText :: ArrowXml a => a XmlTree XmlTree
selectAllText
  -- = deep isText
  -- = deep isElem
  -- = deep ( hasName "protocol" )  --
  -- = deep 
  -- getChildren selects the children...
  = deep $ ( isElem 
      >>> hasName "gmd:CI_OnlineResource"
      -- >>> arr [ getUrl, getProtocol ] 
      >>> getUrl
      -- >>> getText >>> mkText  -- works
      -- let t = getText in
      >>> returnA "hi" 
      -- >>> (_ -> mkelem "html" [] [])
    )


      -- >>> (arr addBrackets)
--      >>> getChildren >>> hasName "gmd:protocol"  )


helloWorld  :: ArrowXml a => a XmlTree XmlTree
helloWorld
    = mkelem "html" []              -- (1)
      [ mkelem "head" []
  [ mkelem "title" []
    [ txt "Hello World" ]     -- (2)
  ]
      , mkelem "body"
  [ sattr "class" "haskell" ] -- (3)
  [ mkelem "h1" []
    [ txt "Hello World" ]     -- (4)
  ]
      ]


