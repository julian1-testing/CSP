
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


selectAllText :: ArrowXml a => a XmlTree XmlTree
selectAllText
  -- = deep isText
  -- = deep isElem
  -- = deep ( hasName "protocol" )  --
  -- = deep 
  -- getChildren selects the children...
  = deep (isElem >>> hasName "gmd:CI_OnlineResource" >>> getChildren >>> hasName "gmd:protocol"  )

