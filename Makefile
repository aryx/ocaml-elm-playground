###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

OPAMS=\
  elm_playground.opam elm_playground_native.opam elm_playground_web.opam\
  elm_playground_native_common.opam\
  elm_playground_software.opam

default: all

# claude: @default (recursive) rather than plain 'dune build' so the
# 'default' alias in examples/js/ and games/js/ is used and their .html
# files are copied into _build/ next to the generated .bc.js
all: $(OPAMS)
	dune build @default
clean:
	dune clean
install:
	dune install

# to test the native programs, run make and then go to
# _build/default/examples/ (or games/) and run the .exe there
# to test the web programs, run also make and then go to
# _build/default/examples/js/ (or games/js/) under chrome for instance
# with open -a "Google Chrome" _build/default/examples/js
# claude: or use 'make serve-build' below, required for the WebGL
# pages with textures.

# claude: build, then serve _build/default/ over HTTP, for the web
# programs, e.g.
#   http://localhost:8001/examples3d/webgl/TexturedCube3d.html
#   http://localhost:8001/examples/js/Mario.html
# Opening their .html directly (file://) works for most of them, but
# not for a WebGL page with textures: WebGL refuses to read the pixels
# of an image it considers from another site (it would let the page
# spy on images it shouldn't see), and Chrome considers every file://
# page a site of its own, even for an image in the same directory. The
# texture then stays magenta, with a SecurityError in the browser's
# console (see playground3d/webgl/Playground3d_platform.ml, Textures).
# Served over HTTP, the page and its images are one site.
# Port 8001, so it can run alongside 'make serve' (docs/, on 8000);
# 127.0.0.1, so only this machine can connect. Ctrl-C to stop.
serve-build: all
	@echo "serving _build/default/ at http://localhost:8001/"
	python3 -m http.server --directory _build/default --bind 127.0.0.1 8001

test:
	dune runtest -f

# after 'make test' reported 2D or 3D golden frames that differ on
# purpose (look at them first), make the new frames the golden ones;
# see tests/common/Testutil_golden.mli
approve-golden2d:
	cp _build/default/tests/2d/actual/*.png tests/2d/golden/
	chmod 644 tests/2d/golden/*.png
approve-golden3d:
	cp _build/default/tests/3d/actual/*.png tests/3d/golden/
	chmod 644 tests/3d/golden/*.png
# the same for audio/'s golden WAVs (listen to them first)
approve-golden-audio:
	cp _build/default/audio/tests/actual/*.wav audio/tests/golden/
	chmod 644 audio/tests/golden/*.wav

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
# claude: website used to 'rm -rf docs' and replace it with the odoc
# output, which also deleted the hand-written parts of docs/
# (index.html, screenshots/, toy-*-example/, claude_notes/,
# examples/ and games/ with their index.html), and 'make js' only builds
# in _build/, so the published examples/games were not updated either.
# Now we replace only the odoc-generated directories (not index.html,
# which is hand-edited, nor the toy-game/toy-web-game docs that odoc also
# generates from docs/toy-*-example/), and copy each freshly built web
# example/game (.bc.js + its .html page) to docs/examples/ and docs/games/.
# claude: and the 3D examples (examples3d/js/) to docs/examples3d/.
# claude: and the WebGL 3D examples/games (examples3d/webgl/,
# games3d/webgl/) to docs/examples3d/webgl/ and docs/games3d/webgl/,
# a subdirectory since the same names exist for both web backends;
# plus the textures, at the path the pages look for them (relative to
# the page, see examples3d/webgl/examples3d/dune).
# 'install -m 644' rather than 'cp' because dune's outputs are read-only.
ODOC_DIRS=odoc.support \
  elm_playground elm_playground_native elm_playground_web\
  elm_playground_native_common elm_playground_software

website:
	make doc
	for d in $(ODOC_DIRS); do \
	  rm -rf docs/$$d; \
	  cp -R _build/default/_doc/_html/$$d docs/$$d; \
	  chmod -R u+w docs/$$d; \
	done
	make js
	for d in examples games examples3d; do \
	  for js in _build/default/$$d/js/*.bc.js; do \
	    b=`basename $$js .bc.js`; \
	    install -m 644 $$js $$d/js/$$b.html docs/$$d/; \
	  done; \
	done
	for d in examples3d games3d; do \
	  mkdir -p docs/$$d/webgl; \
	  for js in _build/default/$$d/webgl/*.bc.js; do \
	    b=`basename $$js .bc.js`; \
	    install -m 644 $$js $$d/webgl/$$b.html docs/$$d/webgl/; \
	  done; \
	done
	mkdir -p docs/examples3d/webgl/examples3d
	install -m 644 examples3d/checker.png docs/examples3d/webgl/examples3d/
	mkdir -p docs/games3d/webgl/games3d
	install -m 644 games3d/texture.png docs/games3d/webgl/games3d/

# Preview the site at http://localhost:8000
serve:
	python3 -m http.server --directory docs 8000

js:
	dune build games/js --profile=release-js
	dune build examples/js --profile=release-js
	dune build examples3d/js --profile=release-js
	dune build examples3d/webgl --profile=release-js
	dune build games3d/webgl --profile=release-js

###############################################################################
# Developer targets
###############################################################################

check:
	osemgrep --config semgrep.jsonnet .

build-docker:
	docker build -t "elm_playground" .
build-docker-ocaml5:
	docker build -t "elm_playground" --build-arg OCAML_VERSION=5.5.1 .

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
	codemap -screen_size 3 -filter xix -efuns_client efuns_client -emacs_client /dev/null .

opendoc:
	dune build @doc
	open _build/default/_doc/_html/index.html
