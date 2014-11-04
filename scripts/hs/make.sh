#/bin/bash

set -o errexit
set -o nounset

OUR_DIR="$( cd "$( dirname "$0" )" && pwd )"


export CORES=${CORES:-0}
export OPTIMISATION=${OPTIMISATION:-"-O1"}
export LLVM=${LLVM:-"llvm-off"}
# export BIN_DIR=${BIN_DIR:-${OUR_DIR}/bin}
export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

AVAILABLE_CORES=$( (grep -c ^processor /proc/cpuinfo 2> /dev/null) || (sysctl hw.logicalcpu | awk '{print $2}' 2> /dev/null) || 0 )

if [ $CORES -le 0 ]; then
    echo "CORES is set to 0. Will try to use all cores on the machine."
    if [ $AVAILABLE_CORES -le 0 ]; then
        echo "Cannot tell how many cores the machine has. Will use only 1."
        USE_CORES=1
    else
        echo "This machine seems to have $AVAILABLE_CORES cores. Will use all."
        USE_CORES=$AVAILABLE_CORES
    fi
else
    USE_CORES=$CORES
    echo "Using ${USE_CORES} cores."
fi


echo "CORES           : ${CORES}"
echo "AVAILABLE_CORES : ${AVAILABLE_CORES}"
echo "USE_CORES       : ${USE_CORES}"
echo "OPTIMISATION    : ${OPTIMISATION}"
echo "LLVM            : ${LLVM}"
echo "BIN_DIR         : ${BIN_DIR}"
echo "CONJURE_REPO"   : ${CONJURE_REPO}

if  [ ! -d ../../../essenceAST/ ]; then
	echo ""
	echo "Requires 'essenceAST' at ../../../essenceAST/"
	echo "i.e. next to the 'instancegen' repo"
	echo "url of 'essenceAST':"
	echo "hg clone ssh://hg@bitbucket.org/stacs_cp/essenceast essenceAST"
	exit 4
fi


if [ $LLVM = "llvm-on" ]; then
    LLVM='--ghc-options="-fllvm"'
else
    LLVM=""
fi



# init sandbox if it doesn't exist

if [ -f cabal.sandbox.config ]; then
    echo "Reusing existing cabal sandbox."
else
    echo "Initialising cabal sandbox."
    cabal sandbox init
    cabal sandbox add-source $CONJURE_REPO
fi

# install finally

cabal install                                                       \
    --only-dependencies                                             \
    --disable-library-profiling --disable-executable-profiling      \
    --force-reinstalls                                              \
 	${LLVM} ${OPTIMISATION} -j"${USE_CORES}"

cabal configure                                                     \
    --disable-library-profiling --disable-executable-profiling      \
    ${LLVM} ${OPTIMISATION} --bindir="${BIN_DIR}"

cabal build -j"${USE_CORES}"
cabal copy                                  # install in ${BIN_DIR}


