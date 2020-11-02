all:
	dune build
clean:
	dune clean
test:
	dune runtest -f
install:
	dune install

.PHONY: all clean install test dump

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .

.PHONY: visual
