#!/bin/bash 

# - setting the module main function explicitly for the executable - avoids Main.o files being generated in tmp and confusing the build.
# - Using -i will pull in dependnecies 

# do we need -make also ?
FLAGS="-i./src -O2 -outputdir tmp"

# needed dirs
[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target

# uncomment to test module in isolation
files=' src/Metadata.hs '
# files=' src/FacetCalc.hs '
# files=' src/Search.hs '
# files=' src/Query.hs '
# files=' src/Service.hs '
# files=' src/Record.hs '
# files=' src/Service.hs '
# files=' src/ParseMCP20.hs '
# files=' src/FreeText.hs '
# files=' src/LoadImage.hs '

# main binaries
# files=' src/LoadSchemes.hs src/Harvest.hs src/Service.hs src/LoadImage.hs '
# files=' src/Metadata.hs src/Service.hs '
# files=' src/Service.hs '


for i in $files; do

  # i                   # eg.  src/Warp.hs 
  f="$(basename $i)"    # Warp.hs
  w="${f%.hs}"          # Warp

  echo $f; 

  ghc $FLAGS -main-is "$w.main" "$i" -o "target/$w" 
done

