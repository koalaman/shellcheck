# TODO: Phase out Makefile in favor of Cabal

GHCFLAGS=-O9

all: shellcheck jsoncheck .tests
	: Done
	
shellcheck: regardless
	: Conditionally compiling shellcheck
	ghc $(GHCFLAGS) --make shellcheck

jsoncheck: regardless
	: Conditionally compiling shellcheck
	ghc $(GHCFLAGS) --make jsoncheck

.tests: *.hs */*.hs
	: Running unit tests
	./test/runQuack && touch .tests

clean:
	rm -f .tests dist shellcheck jsoncheck *.hi *.o  ShellCheck/*.hi ShellCheck/*.o

regardless:
