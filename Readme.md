Building
--------

Requirements

	ghc 7.10+ to compile
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

Command completion for gen (add to .bash_profile, .bashrc or .zshrc as appropriate):

	  source <repo_path>/hs/scripts/_gen.sh

`gen reduce`
--------

Using https://bitbucket.org/stacs_cp/savilerow/issues/136/sr-generates-invalid-minion as an example

Put a file named `spec.essence` in a directory. Add the param as `given.param` if required.

Basic usage (. being the directory with spec.essence), using 30 seconds to each reduction on 1 core.

    gen reduce  -p30 --from-essence . --always-compact        # Don't have choices
    gen reduce  -p30 --from-essence . --choices [eprime|json] # Have choices

You usually want `-N` to hide output of running each specification since it generally not useful,

the `kind` of error can specified e.g.

    gen reduce -p30 --from-essence --always-compact --kind=Savilerow_

Full list of kinds can be found from:

    gen reduce --list-kinds
    gen reduce --list-statuses

This reduces this specification

    language Essence 1.3

    given module_EnumSize: int
    given fin4: int
    given fin5: int
    given fin6: int
    given var2: matrix indexed by [int(1..module_EnumSize), int(1..fin4)] of int(fin5..fin6)
    find b : bool
    such that b = and([true | m1 : int(1..module_EnumSize)
                            , q5 : int(1..fin4)
                            , t1 : int(0..var2[m1, q5] - 1)
                            ])

to this much smaller one.

    language Essence 1.3

    find unused: bool
    such that and([true | q5 : int(1..2), t1 : int(0..[2; int(1..1)][q5])])




`gen essence`
--------


Run for 15 minutes with 60 seconds for the toolchain on 4 cores:

	gen essence -p60 -t900 -c4



Add `-N` to hide the output of the toolchain since it generally not useful, It is still written to file.

Add `-d` to specific the domain depth & `-e` to specific the expression depth.

Add `-w <JSON-FILE>` to specific the weights in below format, anything not specified defaults to 100.

	{"TypeSet":30
	,"TypeInt":53
    }

Use `gen weights` to output example weightings files.

### More examples

Run for 15 minutes with 30 seconds for the refinement, 4 cores:

	gen essence  -p30 -t900 -c4  --mode=refine

Run with 30 seconds for the toolchain on 4 cores using a directory (including sub-directories) of essence files:

	gen json -d <essence-dir>
	gen essence --given <essence-dir> -p30 -c4



`gen instance` Help
-----------------------

Only the required options are shown see  `--help` for more info

    gen instance-undirected [OPTIONS] essence
      Generate discriminating instance for the given essence specification using a
      baseline method

    Required:
      -p    --per-model-time=INT         Time per model
      -i    --iterations=INT             Number of races
      -m    --mode=ITEM                  The suffix of the models directory
      -c    --cores=INT                  Number of cores to use, required unless
                                         CORES is set

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


    gen instance-noRacing [OPTIONS] essence
      Only Generate instances i.e. no racing. Results will be in the _params
      sub-directory of -o

    Required:
      -i    --iterations=INT             Number of instances to generate


    gen instance-summary [OPTIONS]
      Generate a summary of the results, placed in the summary sub-directory.

    Required:
      -o    --output-directory=DIR  Output directory




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


