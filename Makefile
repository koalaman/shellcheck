all: shpell .tests
	: Done
	
shpell: regardless
	: Conditionally compiling shpell
	ghc --make shpell

.tests: *.hs */*.hs
	: Running unit tests
	./test/runQuack && touch .tests

clean:
	rm -f .tests shpell  *.hi *.o  Shpell/*.hi Shpell/*.o

regardless:
	
