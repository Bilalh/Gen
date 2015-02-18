Requires

	Python3.4
	ghc 7.8 to compile
	Compatible Conjure, Savilerow & Minion


Firstly

	git clone git@bitbucket.org:Bilalh/instancegen.git instancegen

To build (places `testSample` in ~/.cabal/bin)

	export CONJURE_LIB=<where the new-conjure repo is>
	make


Before running

	export PARAM_GEN_SCRIPTS=<full path of instancegen/scripts>

Usage from:

	gen
  