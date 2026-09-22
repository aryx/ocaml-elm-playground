###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

# coupling: one per (package ...) stanza of dune-project, which they
# are generated from; ./configure installs the dependencies of all of
# them, so a package missing here would keep a stale .opam around.
OPAMS=\
  elm_playground.opam elm_playground_native.opam elm_playground_web.opam\
  elm_playground_native_common.opam\
  elm_playground_software.opam\
  elm_playground_3d.opam elm_playground_3d_native_common.opam\
  elm_playground_3d_software.opam elm_playground_3d_web.opam\
  elm_playground_3d_opengl.opam elm_playground_3d_webgl.opam

default: all

# claude: @default (recursive) rather than plain 'dune build' so the
# 'default' alias in examples/web/ and games/web/ is used and their .html
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
# _build/default/examples/web/ (or games/web/) under chrome for instance
# with open -a "Google Chrome" _build/default/examples/web
# claude: or use 'make serve-build' below, required for the WebGL
# pages with textures.

# claude: build, then serve _build/default/ over HTTP, for the web
# programs, e.g.
#   http://localhost:8001/examples/web/TexturedCube3d.html
#   http://localhost:8001/examples/web/Mario.html
# Opening their .html directly (file://) works for most of them, but
# not for a WebGL page with textures: WebGL refuses to read the pixels
# of an image it considers from another site (it would let the page
# spy on images it shouldn't see), and Chrome considers every file://
# page a site of its own, even for an image in the same directory. The
# texture then stays magenta, with a SecurityError in the browser's
# console (see the WebGL backend's Playground3d_platform.ml, Textures).
# Served over HTTP, the page and its images are one site.
# Port 8001, so it can run alongside 'make serve' (docs/, on 8000);
# 127.0.0.1, so only this machine can connect. Ctrl-C to stop.
serve-build: all
	@echo "serving _build/default/ at http://localhost:8001/"
	python3 -m http.server --directory _build/default --bind 127.0.0.1 8001

test:
	dune runtest -f

# claude: the quick check for a change that only moves or renames
# things: the whole tree built (what a move breaks: a module in the
# wrong stanza, a missing copy_files, a library dep), and every test
# but the golden frames (a frame rendered on the CPU each, all at
# once) and the ones tagged heavy (seconds of search each, see
# tests/common/Testutil_heavy.mli). 'make test' before a change that
# can alter a pixel or a game.
test-lite:
	dune build
	GOLDEN=none HEAVY=skip dune runtest -f

# 'make test' skips the golden frames deep into a game (more than 100
# frames to render: seconds of CPU each, all at once), keeping every
# example's and the first frame of each game. This runs those too;
# worth it before a release, or after touching a renderer. See
# tests/common/Testutil_golden.mli
test-golden-all:
	GOLDEN=all dune runtest -f tests/2d tests/3d

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
# claude: examples/web/ has the 3D examples on WebGL too; the SVG ones
# (examples/svg/) go to docs/examples/svg/, a subdirectory since they
# have the same names. The WebGL 3D games (games3d/webgl/) go to
# docs/games3d/webgl/. Plus the textures, at the path the pages look
# for them (relative to the page, see examples/web/examples/dune).
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
	for d in examples games games2.5d $(GENRES); do \
	  mkdir -p docs/$$d; \
	  for js in _build/default/$$d/web/*.bc.js; do \
	    b=`basename $$js .bc.js`; \
	    install -m 644 $$js $$d/web/$$b.html docs/$$d/; \
	  done; \
	done
	mkdir -p docs/examples/svg
	for js in _build/default/examples/svg/*.bc.js; do \
	  b=`basename $$js .bc.js`; \
	  install -m 644 $$js examples/svg/$$b.html docs/examples/svg/; \
	done
	for d in games3d; do \
	  mkdir -p docs/$$d/webgl; \
	  for js in _build/default/$$d/webgl/*.bc.js; do \
	    b=`basename $$js .bc.js`; \
	    install -m 644 $$js $$d/webgl/$$b.html docs/$$d/webgl/; \
	  done; \
	done
	mkdir -p docs/examples/examples
	install -m 644 examples/checker.png docs/examples/examples/
	mkdir -p docs/games3d/webgl/games3d
	install -m 644 games3d/texture.png docs/games3d/webgl/games3d/

# Preview the site at http://localhost:8000
serve:
	python3 -m http.server --directory docs 8000

# claude: the games' genres' directories (games/<genre>/, each with its
# own web/), see docs/claude_notes/plans/plan_merge_2d_3d.md
GENRES=games/rhythm

js:
	dune build games/web $(GENRES:%=%/web) --profile=release-js
	dune build games2.5d/web --profile=release-js
	dune build examples/web --profile=release-js
	dune build examples/svg --profile=release-js
	dune build games3d/webgl --profile=release-js
	dune build apps/web --profile=release-js

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
