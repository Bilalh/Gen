install:
	@bash scripts/install.sh

install_and_meta: keys update_ops_metadata install

test:
	@RUN_TESTS=yes bash scripts/install.sh

clean:
	@bash scripts/clean.sh

# Use `make ghci` to run ghci on .hs files
# Or  `make ghci files=<files>` to run on specific files

# Limit the ops to be added added with one of the following
# OPS="Eq Union" make add_ops
# OPS=Eq,Union make add_ops
add_ops:
ifeq ($(ops)x, x)
	@echo "USAGE: make add_ops ops='op1 op2'"
	@echo "USAGE: make add_ops ops=op1,op2"
	@echo "MISSING: make print_missing_ops"
	@echo ""
	@echo "Add all ops from conjure"
	@echo "ops=ALL make add_ops"
else
	runhaskell scripts/add_new_ops.hs "${CONJURE_LIB}/src/Conjure/Language/Expression/Op/" $(subst ALL, ,$(subst ${comma}, ,${ops})) \
	&& cabal exec -- runhaskell scripts/mk_keys.hs
	&& runhaskell scripts/add_ops_metadata.hs
endif

# Show the ops that have not been added yet
print_missing_ops:
	runhaskell scripts/missing_ops.hs ${CONJURE_LIB}/src/Conjure/Language/Expression/Op/

# to use a comma in $(subst you need to put it in a variable
comma :=,

update_ops_metadata:
	runhaskell scripts/add_ops_metadata.hs

keys:
	cabal exec -- runhaskell scripts/mk_keys.hs


# Useful to make an archive that does not require git
autoversion:
	bash scripts/make_autoversion.sh

# Update the cabal  exposed modules automatically
update_exposed_modules:
	./scripts/update_exposed_modules.rb

# Ouput logs when running
trace_install:
	@bash scripts/install.sh --ghc-options="-DGEN_TRACE"


# Nicer test output
failures:
	@BUILD_TESTS=yes bash scripts/install.sh && \
	./dist/build/gen-testing/gen-testing  -j 4 --color always  | grep -v OK

failures_run:
	@./dist/build/gen-testing/gen-testing  -j 4 --color always | grep -v OK


# tags for vim & emacs
etags:
	hasktags --etags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`

conjure_etags:
	cd "${CONJURE_LIB}" && \
	hasktags --etags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`

vtags:
	hasktags --ctags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`

conjure_vtags:
	cd "${CONJURE_LIB}" && \
	hasktags --ctags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`


store:
	# Storing dependency infomation in cabal.config.freeze
	( mv cabal.config cabal.config.org && cabal freeze && mv cabal.config cabal.config.freeze && mv cabal.config.org cabal.config  )

# load ghci with all .hs files if `files` is not specifed
# `args` can be used to specify `-DGEN_TRACE` for tracing logs
# Usage:
# `make ghci` to run ghci on .hs files
# `make ghci files=<files>` to run on specific files
ghci:
	@cabal exec ghci -- -isrc -isrc/test -idist/build/autogen  \
	-XNoImplicitPrelude                        \
		-XOverloadedStrings                        \
		-XScopedTypeVariables                      \
		-XTemplateHaskell                          \
		-XQuasiQuotes                              \
		-XLambdaCase                               \
		-XRecordWildCards                          \
		-XNamedFieldPuns                           \
		-XMultiWayIf                               \
		-XFlexibleContexts                         \
		-XFlexibleInstances                        \
		-fwarn-incomplete-patterns                 \
		-fwarn-incomplete-uni-patterns             \
		-fwarn-missing-signatures                  \
		-fwarn-name-shadowing                      \
		-fwarn-orphans                             \
		-fwarn-overlapping-patterns                \
		-fwarn-tabs                                \
		-fwarn-unused-do-bind                      \
		-fwarn-unused-matches                      \
		-Wall                                      \
		-fghci-hist-size=500                       \
		${args}                                    \
		${files}

# To use a # in a subshell you have to esacpe it twice (for make & for shell)
# to do this we put it in a varible
hash  := \#
files := `find src -name '*.hs' | egrep -v 'exec|test/' | grep -v \${hash}`

ifdef $$files
files := $$files
endif

