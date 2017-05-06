#!/bin/bash 

# do we need -make also ?
FLAGS="-i./src -O2 -outputdir tmp"

[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target


# - setting the module main function explicitly for the executable - avoids Main.o files being generated in tmp and confusing the build.
# - Using -i will pull in dependnecies 


# i=HttpClient.hs
for i in src/*.hs; do

                        # src/Warp.hs 
  f="$(basename $i)"    # Warp.hs
  w="${f%.hs}"          # Warp

  echo $f; 

  ghc $FLAGS -main-is "$w.main" "$i" -o "target/$w" 
done


