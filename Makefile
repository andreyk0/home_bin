#
# Let's learn some basic haskell
#

SRC:=src/haskell

HSFILES:=$(shell find $(SRC) -type f -name '*.hs')

GHC:=ghc -Wall -O2

CABAL_DEPS:=aeson aeson-pretty hoogle shqq missingh

.PHONY: deps tools

tools: pretty-json jar-dups

pretty-json: $(HSFILES)
	$(GHC) -o $@ $(SRC)/PrettyJsonMain.hs

jar-dups: $(HSFILES)
	$(GHC) -o $@ $(SRC)/JarDupsMain.hs

# install cabal dependencies
deps:
	cabal install $(CABAL_DEPS)
