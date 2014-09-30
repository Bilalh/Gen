Requires
	
	Python3.4 (or Python3.3 with enum34 and pathlib)
	Compatible Conjure, Savilerow & Minion

To build (places `testGen` in ./bin)

	export CONJURE_REPO=<where conjure repo is>
	./make.sh


Before running

	export PARAM_GEN_SCRIPTS=<full path of instancegen/scripts>
		
Usage 

	TestGen [OPTIONS]

	Common flags:
	  -b    --base-directory=ITEM  Base Directory
	  -t    --total-time=NUM       Total time to use
	  -p    --per-spec-time=INT    Total time to spend on each spec
	  -r    --rseed=INT            Seed to Use
	  -h -? --help                 Display help message
	  -V    --version              Print version information
	  
Example 

	 testgen -b outdir -t600 -p50 --rseed 4223