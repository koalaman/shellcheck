# TODO: Phase out Makefile in favor of Cabal

GHCFLAGS=-O9

all: shellcheck .tests shellcheck.1
	: Done
	
shellcheck: regardless
	: Conditionally compiling shellcheck
	ghc $(GHCFLAGS) --make shellcheck

.tests: *.hs */*.hs
	: Running unit tests
	./test/runQuack && touch .tests

shellcheck.1: shellcheck.1.md
	pandoc -s -t man $< -o $@

clean:
	rm -f .tests shellcheck shellcheck.1
	rm -f *.hi *.o ShellCheck/*.hi ShellCheck/*.o
	rm -rf dist

regardless:
