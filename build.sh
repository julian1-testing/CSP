#!/bin/bash 

# do we need -make also ?
FLAGS="-i./src -O2 -outputdir tmp"

[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target


# - setting the module main function explicitly for the executable - avoids Main.o files being generated in tmp and confusing the build.
# - Using -i will pull in dependnecies 



# files=' src/FacetRequest.hs src/LoadScheme '
# files=' src/FacetRequest.hs '
# files=src/*.hs
# files=src/CSW.hs
# files=' src/CSP.hs src/FacetCalc.hs '
# files=' src/Metadata.hs src/FacetCalc.hs src/CSP.hs '
# files=' src/CSP.hs src/Metadata.hs '
files=' src/CSP.hs '

for i in $files; do

  # i                     src/Warp.hs 
  f="$(basename $i)"    # Warp.hs
  w="${f%.hs}"          # Warp

  echo $f; 

  ghc $FLAGS -main-is "$w.main" "$i" -o "target/$w" 
done


###############

# - setting the module main function explicitly for the executable - avoids Main.o files being generated in tmp and confusing the build.
# - Using -i will pull in dependnecies 

# ghc $FLAGS -main-is LoadScheme.main  ./src/LoadScheme.hs  -o target/LoadScheme

# ghc $FLAGS -main-is CSW.main  ./src/CSW.hs  -o target/CSW

# ghc $FLAGS -main-is ParseMCP20.main   ./src/ParseMCP20.hs  -o target/ParseMCP20

# ghc $FLAGS -main-is RecordStore.main  ./src/RecordStore.hs  -o target/RecordStore


# ghc $FLAGS -main-is Harvest.main  ./src/Harvest.hs  -o target/Harvest

# here,
# ghc $FLAGS -main-is FacetRequest.main ./src/FacetRequest.hs -o target/FacetRequest

#ghc $FLAGS -main-is OutputMCP.main ./src/FacetRequest.hs -o target/FacetRequest


