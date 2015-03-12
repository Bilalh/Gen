install:
	@bash scripts/install.sh

trace_install:
	@bash scripts/install.sh --ghc-options="-DLOGS_TRACE"

clean:
	@bash scripts/clean.sh

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
	    `find src -name '*.hs' | grep -v 'exec' | grep -v '\.#' `

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

etags:
	hasktags --etags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`

vtags:
	hasktags --ctags --ignore-close-implementation `find -L src -name '*.hs' | grep -v \#`


