###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

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

###############################################################################
# Developer targets
###############################################################################

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
