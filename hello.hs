
import Text.XML.HXT.Core

-- use selem for single element

-- what we want is arg

-- fuck.

genNode :: ArrowXml a => String -> a XmlTree XmlTree
genNode x =  
  mkelem "div" []
     [ txt x ]


helloWorld  :: ArrowXml a => a XmlTree XmlTree
helloWorld
    = mkelem "html" []              -- (1)
      [ mkelem "head" []
        [ mkelem "title" []
          [ txt "Hello World" ]     -- (2)
        ]
      , mkelem "body" [ sattr "class" "haskell" ] -- (3)
        [ mkelem "h1" []
          [ txt "Hello World" ]     -- (4)
        ]

      <+> genNode "hi" 
      <+> genNode "there" 
  ]



-- it might be easier to just use text...
-- 

application = do
  root [] [helloWorld]                         -- (1)
      >>>
      writeDocument [withIndent yes] "hello.xml"   -- (2)
       


main :: IO ()
main = do
  print "hi"

 
  x <- runX (application )

  return ()

