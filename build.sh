#!/bin/bash -x

FLAGS="-O2 -outputdir output"

############

# rm output -rf
# ghc $FLAGS harvest.hs || exit

# rm output -rf
# ghc $FLAGS  load-scheme.hs || exit

[ -d output ] || mkdir output

# we don't want to remove the files in output otherwise we have to build everythign again
# rm Main

ghc $FLAGS Main.hs Facet.hs FacetFormat.hs


