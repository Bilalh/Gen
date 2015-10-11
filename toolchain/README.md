Scripts that are used either by `gen` or `breaking_conjure`
This directory is copy to the cabal data dir,
which can be overwritten by placing this directory next to the `gen` binary


Used by gen essence reduce script-toolchain

* `toolchain.py`  and `toolchain/` - script to run the toolchain
* `save_binaries_csv.sh` - script to save version info
*  savilerow2.sh  - used by toolchain/ savilerow


Used by gen instance*

* wrappers
* tools

Used by `breaking_conjure`

* `eprime_to_choices.py`
* `bin_dir_from.meta.py`
* `reduce_error.py`
* `save_binaries_csv.sh`
* `toolchain.py`  and `toolchain/` - script to run the toolchain
* `update_choices_filter.py`