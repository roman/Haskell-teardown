HS_FOLDERS=$$(find . -maxdepth 3 -type d | grep 'src\|test')

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help

TEST_RESOLVER ?= lts
TEST:=stack build --resolver $(TEST_RESOLVER) --install-ghc --test --haddock --no-haddock-deps --pedantic
test: ## Execute test suite
	$(TEST) teardown:teardown-test
	$(TEST) teardown:teardown-doctest
.PHONY: test

sdist: ## Build a release
	stack sdist . --pvp-bounds both
.PHONY: sdist

DIST_DIR:=$$(stack path --dist-dir)
SDIST_TAR:=$$(find $(DIST_DIR) -name "*.tar.gz" | tail -1)
untar_sdist: sdist
	tar xzf $(SDIST_TAR)
.PHONY: untar_sdist

SDIST_FOLDER:=$$(basename $(SDIST_TAR) .tar.gz)
INIT:=$$(stack init --force)
test_sdist: untar_sdist
	cd $(SDIST_FOLDER) && $(INIT) && $(TEST)
.PHONY: test_sdist

stylish_haskell_install:
	stack --local-bin-path=./bin install stylish-haskell
.PHONY: stylish_haskell_install

STYLISH=./bin/stylish-haskell -i {} \;
pretty: stylish_haskell_install ## Normalize style of source files
	find $(HS_FOLDERS) -name "*.hs" -exec $(STYLISH) && git diff --exit-code
.PHONY: pretty

hlint_install:
	stack --local-bin-path=./bin install hlint
.PHONY: hlint_install

lint: hlint_install ## Execute linter
	hlint $(HS_FOLDERS)
.PHONY: hlint

repl_deps_install:
	stack install pretty-show intero
.PHONY: repl_deps_install

repl: repl_deps_install ## start project repl
	chmod go-w .
	chmod go-w .ghci
	chmod go-w .stack-work/intero || true
	stack ghci
.PHONY: repl
