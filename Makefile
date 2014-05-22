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

CABAL_DEPS:= \
	SafeSemaphore \
	aeson \
	aeson-pretty \
	filemanip \
	haskeline \
	missingh \
	regex-tdfa-text \
	shqq \
	tuple-th \
	vty-ui \
	zip-archive

.PHONY: deps tools

tools: \
	generate-test-seq \
	jar-dups \
	pretty-json

generate-test-seq: $(HSFILES)
	$(GHC) -o $@ $(SRC)/GenerateTestSeqMain.hs

jar-dups: $(HSFILES)
	$(GHC) $(THREADED) -o $@ $(SRC)/JarDupsMain.hs

pretty-json: $(HSFILES)
	$(GHC) -o $@ $(SRC)/PrettyJsonMain.hs

# install cabal dependencies
deps:
	cabal install --verbose $(CABAL_DEPS)

# kill all files not tracked by git
nuke:
	git clean -fdx .
