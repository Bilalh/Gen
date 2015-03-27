Requires

	Python 3.4   (`./scripts/misc/install_python3.4.sh` can install python 3.4 locally as ~/bin/python3 )
	ghc 7.8 to compile
	Compatible Conjure, Savilerow & Minion to run


Firstly

	git clone git@bitbucket.org:stacs_cp/gen.git gen

To build (places `gen` in ~/.cabal/bin)

	cd hs
	export CONJURE_LIB=<where the new-conjure repo is> 
	make


Usage from:

	gen
  
  
Bash completion  (e.g. add to .bash_profile or .bashrc)

	  source hs/scripts/_gen.sh
	  

Examples:

Run for 15 minutes with 10 seconds for the toolchain, discarding passing test cases on 4 cores:

	gen essence  -p10 -t900 -c4 -@DN --size 3
	
Run for 15 minutes with 60 seconds for the refinement, discarding passing test cases on 4 cores:
	
	gen essence  -p60 -t900 -c4 -@DN --size 3 --mode=refine

Run with 30 seconds per spec, discarding passing test cases on 4 cores using a directory of essence files:

	gen json -d <essence-dir>
	gen essence --given <essence-dir> -p30 -c4 -DN 

Run the toolchain on a spec for 4 seconds with 4 cores (compact + 3 randoms):

	gen script-toolchain spec.essence -t40 -c4
	
Rerun the same process again (using the .json files from gen [script-toolchain/essence]) on 1 core:

	gen script-recheck <out_date> -o recheck -c1
	
	