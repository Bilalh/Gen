Requires

	Python3.4
	ghc 7.8 to compile
	Compatible Conjure, Savilerow & Minion


Firstly

	git clone git@bitbucket.org:Bilalh/instancegen.git instancegen

To build (places `gen` in ~/.cabal/bin)

	export CONJURE_LIB=<where the new-conjure repo is>
	make

Usage from:

	gen
  
  
Bash completion 

	  source scripts/_gen.sh