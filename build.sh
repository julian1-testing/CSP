#!/bin/bash -x

rm output -rf
ghc -outputdir output examples/psql.hs || exit

rm output -rf
ghc -outputdir output examples/psql2.hs || exit

rm output -rf
ghc -outputdir output examples/transform.hs || exit

rm output -rf
ghc -outputdir output examples/parse.hs || exit


rm output -rf
ghc -outputdir output harvest.hs || exit

rm output -rf
ghc -outputdir output load-vocab.hs || exit


