#!/bin/bash 

# - setting the module main function explicitly for the executable - avoids Main.o files being generated in tmp and confusing the build.
# - Using -i will pull in dependnecies 

# do we need -make also ?
FLAGS="-i./src -O2 -outputdir tmp"

[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target




# files=' src/CSP.hs src/Metadata.hs '
# files=' src/FacetCalc.hs '
# files=' src/Service.hs '

for i in $files; do

  # i                     src/Warp.hs 
  f="$(basename $i)"    # Warp.hs
  w="${f%.hs}"          # Warp

  echo $f; 

  ghc $FLAGS -main-is "$w.main" "$i" -o "target/$w" 
done


###############

# OLD

# ghc $FLAGS -main-is LoadScheme.main  ./src/LoadScheme.hs  -o target/LoadScheme
# ghc $FLAGS -main-is ParseMCP20.main   ./src/ParseMCP20.hs  -o target/ParseMCP20
# ghc $FLAGS -main-is Harvest.main  ./src/Harvest.hs  -o target/Harvest
# ghc $FLAGS -main-is RecordStore.main  ./src/RecordStore.hs  -o target/RecordStore


