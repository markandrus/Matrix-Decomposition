all: main tests

main: main.hs util.hs
	# We must refer to iconv in /usr/lib
	ghc -L/usr/lib main.hs -o main -fno-cse

tests: tests.hs util.hs
	ghc -L/usr/lib -O2 tests.hs -o tests

.PHONY: all clean

clean:
	rm -f *.hi *.o main tests
