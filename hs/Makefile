install:
	@bash scripts/install.sh

test:
	@RUN_TESTS=yes bash scripts/install.sh

clean:
	@bash scripts/clean.sh

add_ops: 
	runhaskell scripts/add_new_ops.hs "${CONJURE_LIB}/src/Conjure/Language/Expression/Op/"

ops_metadata:
	runhaskell scripts/add_ops_metadata.hs


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

ghci_main:
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
	    -fwarn-incomplete-uni-patterns             \
	    -fwarn-missing-signatures                  \
	    -fwarn-name-shadowing                      \
	    -fwarn-orphans                             \
	    -fwarn-overlapping-patterns                \
	    -fwarn-tabs                                \
	    -fwarn-unused-do-bind                      \
	    -fwarn-unused-matches                      \
	    -Wall                                      \
		src/exec/GenMain.hs
