all:
	dune build --root .
	cp _build/default/Toy.bc.js static
clean:
	dune clean --root .

release:
	dune build --root . --profile=release-js
	cp _build/default/Toy.bc.js static
