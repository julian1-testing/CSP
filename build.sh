#!/bin/bash -x

ghc -outputdir output examples/psql.hs
ghc -outputdir output examples/psql2.hs
ghc -outputdir output examples/transform.hs
ghc -outputdir output examples/parse.hs

ghc -outputdir output client.hs

