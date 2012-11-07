all: shellcheck jsoncheck .tests
	: Done
	
shellcheck: regardless
	: Conditionally compiling shellcheck
	ghc --make shellcheck

jsoncheck: regardless
	: Conditionally compiling shellcheck
	ghc --make jsoncheck

.tests: *.hs */*.hs
	: Running unit tests
	./test/runQuack && touch .tests

clean:
	rm -f .tests shellcheck  *.hi *.o  ShellCheck/*.hi ShellCheck/*.o

regardless:
	
