.PHONY: sandbox init web

all: site

sandbox:
	cabal sandbox init

init: sandbox
	cabal install hakyll

site: site.hs
	cabal exec ghc -- -outputdir _build --make site.hs

web: site
	./site watch
