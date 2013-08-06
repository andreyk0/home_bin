#
# Let's learn some basic haskell
#

SRC:=src/haskell

HSFILES:=$(shell find $(SRC) -type f -name '*.hs')

# http://www.haskell.org/ghc/docs/7.6.2/html/users_guide/template-haskell.html
# http://www.haskell.org/ghc//docs/7.6.2/html/users_guide/prof-heap.html
# http://www.haskell.org/ghc/docs/7.6.2/html/users_guide/profiling.html
GHC_PROF:=-rtsopts -prof -fprof-auto -fprof-auto-calls

GHC:=ghc -i$(SRC) -Wall -O2

THREADED:=-threaded -with-rtsopts="-N"

CABAL_DEPS:=aeson aeson-pretty filemanip hoogle shqq regex-tdfa-text missingh vty-ui zip-archive

.PHONY: deps tools

tools: ifind pretty-json jar-dups

pretty-json: $(HSFILES)
	$(GHC) -o $@ $(SRC)/PrettyJsonMain.hs

jar-dups: $(HSFILES)
	$(GHC) $(THREADED) -o $@ $(SRC)/JarDupsMain.hs

ifind: $(HSFILES)
	$(GHC) --make $(THREADED)                       -o $@      $(SRC)/IFindMain.hs
	#$(GHC) --make $(THREADED) $(GHC_PROF) -osuf p_o -o $@-prof $(SRC)/IFindMain.hs

# install cabal dependencies
deps:
	cabal install --enable-library-profiling $(CABAL_DEPS)

# kill all files not tracked by git
nuke:
	git clean -fdx .
