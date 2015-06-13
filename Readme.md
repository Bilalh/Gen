Building
--------

Requirements

	ghc 7.8+ to compile
	Python 3.4 to run (`./scripts/misc/install_python3.4.sh` can install python 3.4 locally as ~/bin/python3 )
	Compatible Conjure, Savilerow & Minion to run


Firstly

	git clone git@bitbucket.org:stacs_cp/gen.git gen

To build (places `gen` in ~/.cabal/bin)

	cd hs
	export CONJURE_LIB=<where the new-conjure repo is> 
	make


Usage
-----

	gen
	
	gen <subcommand>
  
Command completion (e.g. add to .bash_profile, .bashrc or .zshrc as appropriate):

	  source hs/scripts/_gen.sh
	  

Examples
--------

Add `-N` to hide the output of the  toolchain since it generally not useful.

Add `-d` to specific domain depth & `-e` to specific expression depth.

Add `-w <JSON-FILE>` to specific the weights in below format, anything not specified defaults to 100.

	{"TypeSet":30
	,"TypeInt":53
    }

Use `gen weights` to output example weightings files.


Run for 15 minutes with 60 seconds for the toolchain 4 cores:

	gen essence  -p60 -t900 -c4 
	
Run for 15 minutes with 30 seconds for the refinement 4 cores:
	
	gen essence  -p30 -t900 -c4  --mode=refine

Run with 30 seconds per spec on 4 cores using a directory of essence files:

	gen json -d <essence-dir>
	gen essence --given <essence-dir> -p30 -c4


Run the toolchain on a spec for 4 seconds with 4 cores (compact + 3 randoms):

	gen script-toolchain spec.essence -t40 -c4
	