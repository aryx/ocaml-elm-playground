###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

OPAMS=\
  elm_core.opam elm_system.opam\
  elm_playground.opam elm_playground_native.opam elm_playground_web.opam

default: all

all: $(OPAMS)
	dune build
clean:
	dune clean
install:
	dune install

test:
	dune runtest -f

# This will fail if the .opam isn't up-to-date (in git),
# and dune isn't installed yet. You can always install dune
# with 'opam install dune' to get started.
%.opam: dune-project
	dune build $@

###############################################################################
# Release
###############################################################################

###############################################################################
# Website building
###############################################################################

doc:
	dune build @doc

# Note that I've configured Github Pages for this project at
# https://github.com/aryx/ocaml-elm-playground/settings/pages
# I've selected "Deploy from Branch" "master" and "/docs"
# so it assumes all the html are under docs/.
# Note that if you change the settings, you need to commit in
# the master branch to trigger a redeploy
# TODO: automatically update games/ and examples/
# and add entries for those dirs.
website:
	rm -rf docs
	make doc
	cp -a _build/default/_doc/_html docs
	make js

# Preview the site at http://localhost:8000
serve:
	python -m http.server --directory docs 8000

js:
	dune build games_js --profile=release-js
	dune build examples_js --profile=release-js

###############################################################################
# Developer targets
###############################################################################

check:
	~/zz/bin/osemgrep --experimental --config semgrep.jsonnet .

# To bump-version you need to modify dune-project version then run 'make' then
# commit and merge then:
#  git tag -a 0.1.8
#  git push origin 0.1.8
#  opam publish
# and that's it!
bump:
	echo TODO, see the comment in this file

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
