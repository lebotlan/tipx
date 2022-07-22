.PHONY: all test clean doc build examples

all:	test 

test:
	dune build test/testparse.exe test/testparseformula.exe test/testexploration.exe test/testarrays.exe test/testwalk.exe
	ln -fs _build/default/test/*.exe .

clean:
	find -L . -name "*~" -delete
	rm -f *.exe *.native
	dune clean

