#!/bin/bash 

# - setting the module main function explicitly for the executable - avoids Main.o files being generated in tmp and confusing the build.
# - Using -i will pull in dependnecies 

# do we need -make also ?
FLAGS="-i./src -O2 -outputdir tmp"

[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target




# files=' src/Metadata.hs '
# files=' src/FacetCalc.hs '
files=' src/Search.hs '
# files=' src/Service.hs '


for i in $files; do

  # i                   # eg.  src/Warp.hs 
  f="$(basename $i)"    # Warp.hs
  w="${f%.hs}"          # Warp

  echo $f; 

  ghc $FLAGS -main-is "$w.main" "$i" -o "target/$w" 
done


