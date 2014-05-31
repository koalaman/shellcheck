# TODO: Phase out Makefile in favor of Cabal

GHCFLAGS=-O9
GHCFLAGS_STATIC=$(GHCFLAGS) -optl-static -optl-pthread

all: shellcheck shellcheck.1
	: Done
	
shellcheck: regardless
	: Conditionally compiling shellcheck
	ghc $(GHCFLAGS) --make shellcheck

shellcheck.1: shellcheck.1.md
	: Formatting man page
	pandoc -s -t man $< -o $@

clean:
	rm -f .tests shellcheck shellcheck.1
	rm -f *.hi *.o ShellCheck/*.hi ShellCheck/*.o
	rm -rf dist

shellcheck-static: regardless
	: Conditionally compiling a statically linked shellcheck-static
	ghc $(GHCFLAGS_STATIC) --make shellcheck -o shellcheck-static

regardless:
