###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

OPAMS=elm_core.opam elm_system.opam elm_playground.opam

default: all

all: $(OPAMS)
	dune build
clean:
	dune clean
install:
	dune install

test:
	dune runtest -f
doc:
	dune build @doc

# This will fail if the .opam isn't up-to-date (in git),
# and dune isn't installed yet. You can always install dune
# with 'opam install dune' to get started.
%.opam: dune-project
	dune build $@

###############################################################################
# Developer targets
###############################################################################

check:
	~/zz/bin/osemgrep --experimental --config semgrep.jsonnet .

pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b master
push:
	git push origin `git rev-parse --abbrev-ref HEAD`
merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout master && git pull && git branch -D $$A

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .

opendoc:
	dune build @doc
	open _build/default/_doc/_html/index.html
