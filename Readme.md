Requires

	Python 3.4
	ghc 7.8 to compile
	Compatible Conjure, Savilerow & Minion


Firstly

	git clone git@bitbucket.org:stacs_cp/gen.git gen

To build (places `gen` in ~/.cabal/bin)

	cd hs
	export CONJURE_LIB=<where the new-conjure repo is> 
	make


Usage from:

	gen
  
  
Bash completion 

	  source hs/scripts/_gen.sh
	  

Examples:

Run for 15 minutes with 10 seconds for the toolchain, discarding passing test cases on 4 cores

	gen "out_$(date +%F_%H-%M_%s)"  -p10 -t900 -c4 -@DN --size 3
	
	
Run for 15 minutes with 60 seconds for the refining only, discarding passing test cases on 4 cores	
	
	gen "out_$(date +%F_%H-%M_%s)"  -p60 -t900 -c4 -@DN --size 3 --mode=refine


Run the toolchain on a spec for 4 seconds with 4 cores  (compact + 3 randoms )
	gen script-toolchain spec.essence -t40 -c4 -o "out_$(date +%F_%H-%M_%s)"
	
Rerun the same process again (using the .json files from gen script-toolchain/ gen essence)  on 1 core
	gen script-recheck out_date -o recheck -c1
	
	