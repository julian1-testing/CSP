#!/bin/bash -x

ghc -outputdir output psql.hs
ghc -outputdir output transform.hs
ghc -outputdir output parse.hs

