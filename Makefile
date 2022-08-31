.PHONY: all test clean doc build examples

all:	test tipx

# perf:
# 	dune build --instrument-with landmarks test/testwalkformula.exe
# 	_build/default/test/testwalkformula.exe Nets/DLCround/dlcro_13_b.net Nets/PGCD/formula2.selt


tipx:
	dune build src/cli/tipx.exe
	ln -fs _build/default/src/cli/*.exe .

test:
	dune build test/testparse.exe test/testparseformula.exe test/testexploration.exe test/testarrays.exe test/testwalk.exe test/testwalkformula.exe test/testtfg.exe test/testprojection.exe
	ln -fs _build/default/test/*.exe .

clean:
	find -L . -name "*~" -delete
	rm -f *.exe *.native
	dune clean

