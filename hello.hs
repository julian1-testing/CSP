
import Text.XML.HXT.Core


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


main :: IO ()
main = do
  print "hi"
 
