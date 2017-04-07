#!/bin/bash -x


# do we need -make also ?
FLAGS="-O2 -outputdir tmp"


[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target


# we don't want to remove the files in output otherwise we have to build everythign again
# rm Main

ghc $FLAGS src/Facet.hs src/FacetFormat.hs src/FacetRequest.hs -o target/FacetRequest


# setting the main function explicitly avoids Main.o files being generated in tmp and confusing the build.
ghc $FLAGS -main-is Harvest.main src/Record.hs src/Harvest.hs src/CSW.hs src/Helpers.hs -o target/Harvest 


# record decoding test
ghc -main-is Record.main -outputdir tmp src/Record.hs src/Helpers.hs -o ./target/Record






# compile Facet test code,
# ghc -main-is Facet.main -outputdir tmp Facet.hs  -o ./Facet

# Format only...
# ghc -main-is Facet.main -outputdir tmp  FacetFormat.hs  -o ./FacetFormat

############

# rm output -rf
# ghc $FLAGS  load-scheme.hs || exit

