#!/bin/bash -x


# do we need -make also ?
FLAGS="-O2 -outputdir tmp"


[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target


# setting the main function explicitly avoids Main.o files being generated in tmp and confusing the build.
# we don't want to remove the files in output otherwise we have to build everythign again
# rm Main

# ghc $FLAGS src/Facet.hs src/FacetFormat.hs src/FacetRequest.hs -o target/FacetRequest



# ghc $FLAGS -main-is Harvest.main src/MetadataRecord.hs src/Harvest.hs src/CSW.hs src/Helpers.hs -o target/Harvest 


# record decoding test
ghc -main-is MetadataRecord.main -outputdir tmp src/MetadataRecord.hs src/Helpers.hs -o ./target/MetadataRecord






# compile Facet test code,
# ghc -main-is Facet.main -outputdir tmp Facet.hs  -o ./Facet

# Format only...
# ghc -main-is Facet.main -outputdir tmp  FacetFormat.hs  -o ./FacetFormat

############

# rm output -rf
# ghc $FLAGS  load-scheme.hs || exit

