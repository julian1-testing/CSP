#!/bin/bash -x


# do we need -make also ?
FLAGS="-O2 -outputdir tmp"


[ -d tmp ] || mkdir tmp


# we don't want to remove the files in output otherwise we have to build everythign again
# rm Main

ghc $FLAGS Facet.hs Main.hs FacetFormat.hs


# setting the main function explicitly avoids Main.o files being generated in tmp and confusing the build.
ghc $FLAGS -main-is Harvest.main Harvest.hs



# compile Facet test code,
# ghc -main-is Facet.main -outputdir tmp Facet.hs  -o ./Facet

# Format only...
# ghc -main-is Facet.main -outputdir tmp  FacetFormat.hs  -o ./FacetFormat

############

# rm output -rf
# ghc $FLAGS harvest.hs || exit

# rm output -rf
# ghc $FLAGS  load-scheme.hs || exit

