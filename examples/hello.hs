
import Text.XML.HXT.Core

{-
  attempt to combine the mkelem stuff to create the structure...

-}
-- use selem for single element

-- what we want is arg

-- fuck.

genNode :: ArrowXml a => String -> a XmlTree XmlTree
genNode x =  
  mkelem "div" []
     [ txt x ]

-- where b is a list 

myMap :: ArrowXml a => String -> [ String]  -> a XmlTree XmlTree

myMap e lst =
  let a : b = lst in
  let newLst = a <+> e in
  myMap e b
  

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
      <+> (map genNode [ "there"  ] )

  ]


-- recursion is probably going to be really hard on this - because of the need to pass the root element/node down to insert under
-- because we're making a nested tree...



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

