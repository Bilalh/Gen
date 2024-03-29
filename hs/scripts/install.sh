#/bin/bash

set -o errexit
set -o nounset

export CORES=${CORES:-0}
export OPTIMISATION=${OPTIMISATION:-"-O1"}
export LLVM=${LLVM:-"llvm-off"}
export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}
export RUN_TESTS=${RUN_TESTS:-no}
export BUILD_TESTS=${BUILD_TESTS:-${RUN_TESTS:-no}}


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
echo "CONJURE_LIB     : ${CONJURE_LIB}"
echo "BUILD_TESTS     : ${BUILD_TESTS}"
echo "RUN_TESTS       : ${RUN_TESTS}"
echo

if [ "$LLVM" = "llvm-on" ]; then
    LLVM='--ghc-options="-fllvm"'
else
    LLVM=""
fi


if [ "$BUILD_TESTS" = "yes" ]; then
    TESTS="--enable-tests"
else
    TESTS=""
fi


# init sandbox if it doesn't exist

if [ -f cabal.sandbox.config ]; then
    echo "Reusing existing cabal sandbox."
	if [ -z "${CABAL_UPDATE_SKIP:-}" ]; then
		cabal update
	fi
else
    echo "Initialising cabal sandbox."
    cabal sandbox init
	cabal update
    cabal sandbox add-source "$CONJURE_LIB"
fi

# install finally

if (cabal --version | grep 1.20 ); then
    # profiling="--disable-library-profiling --disable-executable-profiling"
    profiling="--disable-library-profiling"
else
    profiling="--disable-library-profiling --disable-profiling"

fi

# defs='--ghc-options="-DDEFINE_getAllFilesWithSuffix"'
defs=''

cabal install           \
    --only-dependencies \
    ${profiling}	\
    --force-reinstalls  \
 	${TESTS} ${LLVM} ${OPTIMISATION} -j"${USE_CORES}"

cabal configure         \
    ${profiling}	\
    ${defs} ${TESTS} ${LLVM} ${OPTIMISATION} --bindir="${BIN_DIR}"

cabal build -j"${USE_CORES}" ${defs}
cabal copy                                  # install in ${BIN_DIR}

if [ "$RUN_TESTS" = "yes" ]; then
    time dist/build/gen-testing/gen-testing --color always --hide-successes -j "${TEST_CORES:-${USE_CORES:-1}}" --quickcheck-tests "${QC_NUM:-100}"  +RTS -s
fi

pushd "${BIN_DIR}"
ln -sf conjure conjureNew
popd
