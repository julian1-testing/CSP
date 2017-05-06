#!/bin/bash 


# do we need -make also ?
FLAGS="-i./src -O2 -outputdir tmp"


[ -d tmp ] || mkdir tmp
[ -d target ] || mkdir target


# - setting the module main function explicitly for the executable - avoids Main.o files being generated in tmp and confusing the build.
# - Using -i will pull in dependnecies 


# i=HttpClient.hs


for i in src/*.hs; do

  f=$(basename $i)
  echo $f; 

  ghc $FLAGS -main-is "${f%.hs}.main" "src/$f"  -o   "target/${f%.hs}" 
done



############


#### OLD

