all	:
	@echo "available targets: init, delete"

init	:
	cabal sandbox init
	cabal sandbox add-source ../data-size
	cabal sandbox add-source ../ghc-heap-view
	cabal sandbox add-source ../text
	cabal install --dependencies-only --enable-tests

delete	:
	cabal sandbox delete

.PHONY	: init delete
