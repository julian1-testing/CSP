#!/bin/bash -x



FLAGS="-O2 -outputdir output"

############
rm output -rf
ghc $FLAGS harvest.hs || exit


rm output -rf
ghc $FLAGS  load-scheme.hs || exit

rm output -rf
ghc $FLAGS recurse.hs || exit


