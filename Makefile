all	:
	@echo "available targets: push pull"

init	:
	cabal sandbox init
	cabal sandbox add-source ../bytestring		# local version of latest bytesting package
	cabal sandbox add-source ../text		# local version of latest text package
	cabal sandbox add-source ../data-size		# development version of data-source

depends	:
	cabal install --dependencies-only --force-reinstall

clean	:
	cabal sandbox delete

push	:
	git push --tags origin master

pull	:
	git pull --rebase --tags origin master

.PHONY	: all init push pull clean
