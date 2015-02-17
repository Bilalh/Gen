Requires

	Python3.4
	ghc 7.8 to compile
	Compatible Conjure, Savilerow & Minion


Firstly

	git clone git@bitbucket.org:Bilalh/instancegen.git instancegen

To build (places `testSample` in ~/.cabal/bin)

	export CONJURE_REPO=<where conjure repo is>
	./make.sh


Before running

	export PARAM_GEN_SCRIPTS=<full path of instancegen/scripts>


Usage from:

	testSample --help

wrapper scripts, which makes sure the directory exists and saves version information.

	testSampleWrap.sh
	
example:

	testSampleWrap.sh  runs  -t 21600 -p 120 --size 5


save binaries:

	$PARAM_GEN_SCRIPTS/toolchain/save_binaries.sh zbin@

Change the number of cores used (uses all by default)

export CORES=<num_cores>