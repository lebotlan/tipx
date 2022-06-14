.PHONY: all tests clean doc build examples install

build:
	dune build

all:	build examples tests doc


examples:
	dune build @examples/examples
	ln -fs _build/default/examples/*.exe .

tests:
	dune build @tests
#	ln -fs _build/default/*.exe .

clean:
	find -L . -name "*~" -delete
	dune clean
	rm -f *.exe
	rm -rf docs/*


