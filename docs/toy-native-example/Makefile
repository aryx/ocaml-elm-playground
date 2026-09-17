all:
	dune build --root .
	mkdir -p bin
	install -m 0755 _build/default/Toy.exe bin/
clean:
	dune clean --root .
	rm -f bin/Toy.exe
