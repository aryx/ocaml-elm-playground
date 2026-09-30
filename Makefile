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
  elm_playground_software.opam\
  elm_playground_3d.opam\
  elm_playground_3d_software.opam elm_playground_3d_web.opam\
  elm_playground_3d_opengl.opam elm_playground_3d_webgl.opam

default: all

# claude: @default (recursive) rather than plain 'dune build' so the
# 'default' alias in examples/web/ and games/*/web/ is used and their .html
# files are copied into _build/ next to the generated .bc.js
all: $(OPAMS)
	dune build @default
clean:
	dune clean
install:
	dune install

# to test the native programs, run make and then go to
# _build/default/examples/ (or games/<genre>/) and run the .exe there
# to test the web programs, run also make and then go to
# _build/default/examples/web/ (or games/<genre>/web/) under chrome for instance
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
	cp _build/default/libs/audio/tests/actual/*.wav libs/audio/tests/golden/
	chmod 644 libs/audio/tests/golden/*.wav
# and the music applications' voices' (apps/music/tests)
approve-golden-music:
	cp _build/default/apps/music/tests/actual/*.wav apps/music/tests/golden/
	chmod 644 apps/music/tests/golden/*.wav
# and the formats' players' (audio/formats/tests)
approve-golden-formats:
	cp _build/default/libs/audio/formats/tests/actual/*.wav libs/audio/formats/tests/golden/
	chmod 644 libs/audio/formats/tests/golden/*.wav

# claude: tests/data/'s toy media, one file per format we read, remade
# from Our_media.playlist; to try a reader by hand, e.g.
#   dune exec apps/media/TinyMediaPlayer.exe -- tests/data/bell.wav
test-data:
	dune build ./apps/media/tests/Dump_media.exe
	mkdir -p tests/data
	./_build/default/apps/media/tests/Dump_media.exe tests/data

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
# claude: website used to 'rm -rf docs' and replace it with the odoc
# output, which also deleted the hand-written parts of docs/
# (index.html, screenshots/, toy-*-example/, claude_notes/,
# examples/ and games/ with their index.html), and 'make js' only builds
# in _build/, so the published examples/games were not updated either.
# Now we replace only the odoc-generated directories (not index.html,
# which is hand-edited, nor the toy-game/toy-web-game docs that odoc also
# generates from docs/toy-*-example/), and copy each freshly built web
# example/game (.bc.js + its .html page) to docs/examples/ and docs/games/.
# claude: a genre's games go to docs/games/<genre>/ (3D ones on WebGL
# too); examples/web/ has the 3D examples on WebGL too; the SVG ones
# (examples/svg/) go to docs/examples/svg/, a subdirectory since they
# have the same names. Plus the texture, at the path the pages look for
# it (relative to the page, see examples/web/examples/dune); the games'
# textures travel inside their programs.
# 'install -m 644' rather than 'cp' because dune's outputs are read-only.
# claude: since docs/index.html is not regenerated, its package version
# numbers are refreshed from dune-project instead.
# claude: the games' and apps' programs, 200 of them (44 MB), are not
# committed here but in the assets repository, served by its own GitHub
# Pages: games/arcade/web/TinyPong.bc.js goes to
# $(ASSETS)/js/games/arcade/, and its page, docs/games/arcade/TinyPong.html,
# loads it from $(ASSETS_URL)/js/games/arcade/. Commit and push the
# assets first, so that no page points at a program not yet online.
# The index pages of docs/games/, docs/apps/ and docs/examples/ are
# generated (launcher/website/make_website.ml, from CATALOG.md), with
# each program's thumbnail, tinybox's, in $(ASSETS)/pngs/; these
# directories are emptied first, so that a program gone leaves no page.
# And tinybox's menu for the web (launcher/web/, plan_tinybox_web.md),
# docs/tinybox.html, its program beside the others in the assets: a
# program chosen there is its own page, docs/<dir>/<Name>.html; and
# beside it the repository's sources for its code map (10 MB, fetched
# when first needed), written from the source tree.
VERSION=$(shell sed -n 's/^(version "\(.*\)")/\1/p' dune-project)
ASSETS ?= $(HOME)/github/assets
ASSETS_URL=https://aryx.github.io/assets
# claude: the site's icon (docs/favicon.svg), by URL so that a page at any
# depth, or another project's code map, finds it
FAVICON=<link rel="icon" href="https://aryx.github.io/ocaml-elm-playground/favicon.svg" type="image/svg+xml">
# claude: the code map's own icon, a treemap (docs/codemap-favicon.svg), on
# its page here and on the ones it makes for other projects
CODEMAP_FAVICON=<link rel="icon" href="https://aryx.github.io/ocaml-elm-playground/codemap-favicon.svg" type="image/svg+xml">
ODOC_DIRS=odoc.support \
  elm_playground elm_playground_native elm_playground_web\
  elm_playground_software elm_playground_3d elm_playground_3d_software

website:
	make doc
	for d in $(ODOC_DIRS); do \
	  rm -rf docs/$$d; \
	  cp -R _build/default/_doc/_html/$$d docs/$$d; \
	  chmod -R u+w docs/$$d; \
	done
	perl -pi -e 's|<span class="version">[^<]*</span>|<span class="version">$(VERSION)</span>|' docs/index.html
	make js
	rm -rf docs/examples docs/games docs/apps
	mkdir -p docs/examples docs/games docs/apps $(ASSETS)/pngs
	for js in _build/default/examples/web/*.bc.js; do \
	  b=`basename $$js .bc.js`; \
	  install -m 644 $$js examples/web/$$b.html docs/examples/; \
	done
	for d in $(GENRES) $(APPS); do \
	  a=js/$$d; \
	  mkdir -p docs/$$d $(ASSETS)/$$a; \
	  for js in _build/default/$$d/web/*.bc.js; do \
	    b=`basename $$js .bc.js`; \
	    install -m 644 $$js $(ASSETS)/$$a/; \
	    install -m 644 $$d/web/$$b.html docs/$$d/; \
	    perl -pi -e "s|src=\"$$b.bc.js\"|src=\"$(ASSETS_URL)/$$a/$$b.bc.js\"|" docs/$$d/$$b.html; \
	    perl -pi -e 's|<head>|<head>\n    $(FAVICON)|' docs/$$d/$$b.html; \
	  done; \
	done
	mkdir -p docs/examples/svg
	for js in _build/default/examples/svg/*.bc.js; do \
	  b=`basename $$js .bc.js`; \
	  install -m 644 $$js examples/svg/$$b.html docs/examples/svg/; \
	done
	mkdir -p docs/examples/examples
	install -m 644 examples/checker.png docs/examples/examples/
	dune build launcher/codegen/make_tinybox_data.exe launcher/website/make_website.exe
	./_build/default/launcher/codegen/make_tinybox_data.exe pngs $(ASSETS)/pngs
	./_build/default/launcher/website/make_website.exe $(ASSETS_URL)
	mkdir -p $(ASSETS)/js/launcher
	install -m 644 _build/default/launcher/web/Tinybox_web.bc.js $(ASSETS)/js/launcher/
	dune build launcher/codegen/make_codemap_data.exe
	./_build/default/launcher/codegen/make_codemap_data.exe -tinybox > $(ASSETS)/js/launcher/tinybox_sources.txt
	printf '<html>\n  <head>\n    %s\n    <script src="%s"></script>\n  </head>\n  <body>\n  </body>\n</html>\n' \
	  '$(FAVICON)' $(ASSETS_URL)/js/launcher/Tinybox_web.bc.js > docs/tinybox.html
	make codemap-web DIR=. PAGE=docs NAME=ocaml-elm-playground

# claude: make website, then both repositories committed and pushed: the
# assets first (the programs, the thumbnails), so that no page points at
# a program not yet online, then this one -- only what make website
# writes, not what else docs/ may hold (a screenshot not yet committed).
# Nothing committed where nothing changed. And nothing published but what
# is committed: make website builds from the working copy, so a program
# being written there (another session's, its row already in CATALOG.md)
# would go out with its page and its code map, its source not on GitHub.
WEBSITE_PATHS=$(addprefix docs/,$(ODOC_DIRS) examples games apps by-size index.html style.css favicon.svg codemap-favicon.svg tinybox.html codemap.html)
publish:
	@if [ -n "$$(git status --porcelain -- . ':!docs')" ]; then \
	  echo "make publish: changes not committed outside docs/ (git status): commit them, or stash them, first"; \
	  exit 1; \
	fi
	make website
	git -C $(ASSETS) add -A js pngs codemap
	git -C $(ASSETS) diff --cached --quiet || git -C $(ASSETS) commit -q -m "make publish: the programs of $$(git rev-parse --short HEAD)"
	git -C $(ASSETS) push -q origin HEAD
	git add -A $(WEBSITE_PATHS)
	git diff --cached --quiet -- $(WEBSITE_PATHS) || git commit -q -m "website: make publish" -- $(WEBSITE_PATHS)
	git push -q origin HEAD

# Preview the site at http://localhost:8000
serve:
	python3 -m http.server --directory docs 8000

# claude: the games' genres' directories (games/<genre>/, each with its
# own web/), in CATALOG.md's order
GENRES=$(addprefix games/,shmup fighting platform arcade puzzle cards \
  adventure rpg fps flight racing sports strategy rhythm programming)

# claude: the apps' categories' directories (apps/<category>/, each with
# its own web/)
APPS=$(addprefix apps/,office graphics gamedev cad education devtools \
  internet media music pim system)

js:
	dune build $(GENRES:%=%/web) $(APPS:%=%/web) --profile=release-js
	dune build examples/web --profile=release-js
	dune build examples/svg --profile=release-js
	dune build launcher/web --profile=release-js

# claude: a directory's code map as a web page, for a project's site (the
# author's ix, xix, principia): make codemap-web DIR=~/github/ix
# PAGE=~/github/ix/docs. The big files go to the assets repository, as
# the games' (not to pollute the project's): $(ASSETS)/js/codemap/
# codemap.bc.js (the map, the same for every project,
# launcher/codemap/web) and $(ASSETS)/codemap/<name>.txt (the directory's
# code, make_codemap_data); the project's site gets only codemap.html,
# which names them. A link to codemap.html?focus=<path>, &line=<n> or
# ?def=<name> opens the map there; to try it before the assets are
# pushed: ?data=<a local bundle>. NAME defaults to DIR's name.
NAME ?= $(notdir $(abspath $(DIR)))
codemap-web:
	@test -n "$(DIR)" -a -n "$(PAGE)" || (echo "usage: make codemap-web DIR=<project> PAGE=<its site's dir> [NAME=<name>]"; exit 2)
	dune build launcher/codemap/web/Codemap_web.bc.js launcher/codegen/make_codemap_data.exe --profile=release-js
	mkdir -p $(ASSETS)/js/codemap $(ASSETS)/codemap $(PAGE)
	install -m 644 _build/default/launcher/codemap/web/Codemap_web.bc.js $(ASSETS)/js/codemap/codemap.bc.js
	./_build/default/launcher/codegen/make_codemap_data.exe $(DIR) $(NAME) > $(ASSETS)/codemap/$(NAME).txt
	printf '<!DOCTYPE html>\n<html>\n  <head>\n    <meta charset="utf-8">\n    <title>%s: code map</title>\n    %s\n    <style>body { margin: 0; background: #0e0c1c; }</style>\n    <script>var codemap_data = "%s";</script>\n    <script src="%s"></script>\n  </head>\n  <body>\n  </body>\n</html>\n' \
	  $(NAME) '$(CODEMAP_FAVICON)' $(ASSETS_URL)/codemap/$(NAME).txt $(ASSETS_URL)/js/codemap/codemap.bc.js > $(PAGE)/codemap.html

###############################################################################
# Developer targets
###############################################################################

check:
	osemgrep --config semgrep.jsonnet .

# lines of OCaml: library, games, apps, launcher, examples, tests (loc-v: per
# subdirectory)
loc:
	scripts/stats/loc.py
loc-v:
	scripts/stats/loc.py -v

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
