# Makefile for Homework 6
# by Mark <andrus@uchicago.edu>

ghc = ghc
flags = -O2

# NOTE: the following hack is a consequence of GHC for OS X being compiled against the libiconv in
# `/usr/lib`
extra_libs = -L/usr/lib

bins = main tests
deps = Options.hs PowerMethod.hs SVD.hs Utils.hs

all: $(bins)

main: Main.hs $(deps)
	$(ghc) $(extra_libs) $(flags) $< -o $@

tests: tests.hs $(deps)
	$(ghc) $(extra_libs) $(flags) $< -o $@

.PHONY: all clean

clean:
	rm -f *.hi *.o $(bins)
