Building
--------

Requirements

	ghc 7.10+ to compile  (may work on 7.8)
	Python 3.4+ to run (`./scripts/misc/install_python3.4.sh` can install python 3.4 locally as ~/bin/python3 )
	Compatible Conjure, Savilerow & Minion to run


Firstly

	git clone git@bitbucket.org:stacs_cp/gen.git gen

To build (places `gen` in ~/.cabal/bin)

	cd hs
	export CONJURE_LIB=<where the conjure-private repo is>
	make


Usage
-----

	gen

	gen <subcommand>

Command completion (e.g. add to .bash_profile, .bashrc or .zshrc as appropriate):

	  source <repo_path>/hs/scripts/_gen.sh


`gen essence` Examples
--------

Add `-N` to hide the output of the toolchain since it generally not useful, It is still written to file.

Add `-d` to specific the domain depth & `-e` to specific the expression depth.

Add `-w <JSON-FILE>` to specific the weights in below format, anything not specified defaults to 100.

	{"TypeSet":30
	,"TypeInt":53
    }

Use `gen weights` to output example weightings files.


Run for 15 minutes with 60 seconds for the toolchain, 4 cores:

	gen essence  -p60 -t900 -c4

Run for 15 minutes with 30 seconds for the refinement, 4 cores:

	gen essence  -p30 -t900 -c4  --mode=refine

Run with 30 seconds for the toolchain on 4 cores using a directory (including sub-directories) of essence files:

	gen json -d <essence-dir>
	gen essence --given <essence-dir> -p30 -c4


Run the toolchain on a spec for 4 seconds with 4 cores (compact + 3 randoms):

	gen script-toolchain spec.essence -t40 -c4


`gen instance` Example
----------------------

Only the required options are shown see the `--help` for more info

    gen instance-nsample [OPTIONS] essence
      Generate discriminating instance for the given essence specification using
      the nsample method

    Required:
      -p    --per-model-time=INT         Time per model
      -i    --iterations=INT             Number of races
      -m    --mode=ITEM                  The suffix of the models directory
      -c    --cores=INT                  Number of cores to use, required unless
                                         CORES is set
      -f    --influence_radius=INT       Number of races


    gen instance-undirected [OPTIONS] essence
      Generate discriminating instance for the given essence specification using a
      baseline method

    Required:
      -p    --per-model-time=INT         Time per model
      -i    --iterations=INT             Number of races
      -m    --mode=ITEM                  The suffix of the models directory
      -c    --cores=INT                  Number of cores to use, required unless
                                         CORES is set

    gen instance-noRacing [OPTIONS] essence
      Only Generate instances i.e. no racing. Results will be in the _params
      sub-directory of -o

    Required:
      -i    --iterations=INT             Number of instances to generate


    gen instance-allsols [OPTIONS] essence
      Generate the *script* and data required to create all solutions. This will
      usually requires TB(s) of space


    gen instance-summary [OPTIONS]
      Generate a summary of the results, placed in the summary sub-directory.

    Required:
      -o    --output-directory=DIR  Output directory

`gen reduce` Examples
-------------------

Only some options are shown see the `--help` for more info

    gen reduce [OPTIONS] SPEC-DIR
      Reduces a .spec.json file

    Required:
      -p    --per-spec-time=INT          Time per Spec

    Other:
    --from-essence               Convert spec.essence to json
                                 (spec.spec.json), automatically for
                                 convenience



Instance Experiments
--------------------

See https://bitbucket.org/stacs_cp/essence-refinements to see how to run the experiments



Instance Experiments Results
----------------------------

The results of the instance experiments is hosted on the host server since bitbucket limit is 2GB

    ssh://bh246@bh246.host.cs.st-andrews.ac.uk/cs/home/bh246/git/instance_experiments.git


Note this does not contain the .minion files or the .aux files


`scripts` should symlinked to `gen/scripts/instance_results`
`r` should be  symlinked to `gen/scripts/instance_r`


run `./scripts/sync_from_server_no_large.sh <server>` to get results from `<server>`
    assumes that the results on  ~/sampling on the `<server>`

run `./scripts/process_new_results.sh` when new results are synced to generated the resulting data.


