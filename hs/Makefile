install:
	@bash scripts/install.sh

install_and_meta: keys update_ops_metadata install

test:
	@RUN_TESTS=yes bash scripts/install.sh

clean:
	@bash scripts/clean.sh

# Limit the ops to be added added with one of the following
# OPS="Eq Union" make add_ops
# OPS=Eq,Union make add_ops
add_ops:
ifeq ($(OPS)x, x)
	@echo "USAGE: OPS=op1,op2 make add_ops"
	@echo "USAGE: OPS='op1 op2' make add_ops"
	@echo "MISSING: make print_missing_ops"
	@echo ""
	@echo "Add all missing ops "
	@echo "OPS='' make add_ops"
else
	runhaskell scripts/add_new_ops.hs "${CONJURE_LIB}/src/Conjure/Language/Expression/Op/" $(subst ${comma}, ,${OPS}) \
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

autoversion:
	# Useful to make an archive that does not require git
	bash scripts/make_autoversion.sh

exposed_modules_to_clipboard:
	./scripts/make_exposed_modules.sh


failures:
	@BUILD_TESTS=yes bash scripts/install.sh && \
	./dist/build/gen-testing/gen-testing  -j 4 --color always  | grep -v OK

failures_run:
	@./dist/build/gen-testing/gen-testing  -j 4 --color always | grep -v OK


trace_install:
	@bash scripts/install.sh --ghc-options="-DLOGS_TRACE"

etags:
	hasktags --etags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`

conjure_etags:
	cd "${CONJURE_LIB}" && \
	hasktags --etags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`

vtags:
	hasktags --ctags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`


store:
	# Storing dependency infomation in cabal.config.freeze
	( mv cabal.config cabal.config.org && cabal freeze && mv cabal.config cabal.config.freeze && mv cabal.config.org cabal.config  )

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
		`find src -name '*.hs' | egrep -v 'exec|test/' | grep -v '\.#' `

ghci_file:
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
		-fwarn-incomplete-patterns                 \
		-XFlexibleInstances                        \
		-fwarn-incomplete-uni-patterns             \
		-fwarn-missing-signatures                  \
		-fwarn-name-shadowing                      \
		-fwarn-orphans                             \
		-fwarn-overlapping-patterns                \
		-fwarn-tabs                                \
		-fwarn-unused-do-bind                      \
		-fwarn-unused-matches                      \
		-Wall                                      \
		${files}
