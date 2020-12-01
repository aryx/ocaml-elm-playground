all:
	dune build
clean:
	dune clean
test:
	dune runtest -f
install:
	dune install
doc:
	dune build @doc

.PHONY: all clean install test dump

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .

opendoc:
	dune build @doc
	open _build/default/_doc/_html/index.html

.PHONY: visual
