#!/bin/bash -x


# do we need -make also ?
FLAGS="-i./src -O2 -outputdir tmp"


[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target


# - setting the module main function explicitly for the executable - avoids Main.o files being generated in tmp and confusing the build.
# - Using -i will pull in dependnecies 

# ghc $FLAGS -main-is LoadScheme.main  ./src/LoadScheme.hs  -o target/LoadScheme

# ghc $FLAGS -main-is CSW.main  ./src/CSW.hs  -o target/CSW

# ghc $FLAGS -main-is ParseMCP20.main   ./src/ParseMCP20.hs  -o target/ParseMCP20

# ghc $FLAGS -main-is RecordStore.main  ./src/RecordStore.hs  -o target/RecordStore


# ghc $FLAGS -main-is Harvest.main  ./src/Harvest.hs  -o target/Harvest

ghc $FLAGS -main-is FacetRequest.main ./src/FacetRequest.hs -o target/FacetRequest


############


#### OLD

