#
# Let's learn some basic haskell
#

SRC:=src/haskell

HSFILES:=$(shell find $(SRC) -type f -name '*.hs')

GHC_PROF:=-prof -fprof-auto

GHC:=ghc -Wall -O2

CABAL_DEPS:=aeson aeson-pretty hoogle shqq missingh zip-archive

.PHONY: deps tools

tools: pretty-json jar-dups

pretty-json: $(HSFILES)
	$(GHC) -o $@ $(SRC)/PrettyJsonMain.hs

jar-dups: $(HSFILES)
	$(GHC) -threaded -with-rtsopts="-N" -o $@ $(SRC)/JarDupsMain.hs

# install cabal dependencies
deps:
	cabal install $(CABAL_DEPS)
