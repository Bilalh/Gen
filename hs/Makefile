install:
	@bash scripts/install.sh

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
	    `find src -name '*.hs' | grep -v 'exec' | grep -v 'EssenceSolver'  | grep -v '\.#' `

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
