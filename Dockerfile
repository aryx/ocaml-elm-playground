# Build and test the OCaml Elm Playground with OCaml 4.14.4 via OPAM on Ubuntu.
# See also .github/workflows/docker.yml for its use in Github Actions (GHA).
#
# elm_playground_native.opam used to pin ocaml < 5.2.0 (tsdl failed there
# with some missing unix error), but that's fixed upstream now (verified
# 2026-09-16 that OCaml 5.2.0 through 5.5.1 all build and pass the tests) -
# see 'make build-docker-ocaml5', which tries the latest OCaml 5.

# claude: 24.04, not 22.04: tsdl calls SDL_RenderGetWindow, SDL 2.0.22's,
# and 22.04 has SDL 2.0.20 ("undefined symbol" as tinybox list runs)
FROM ubuntu:24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf

# Setup OPAM and OCaml
RUN apt-get install -y opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
ARG OCAML_VERSION=4.14.4
RUN opam switch create ${OCAML_VERSION} -v

# System deps of the native (SDL2 + cairo) backend.
# coupling: elm_playground_native.opam (tsdl, cairo2, ocurl)
RUN apt-get install -y pkg-config libsdl2-dev libcairo2-dev libcurl4-gnutls-dev
# claude: and the system OpenGL, for tgls (elm_playground_3d_opengl.opam),
# which ./configure checks for too
RUN apt-get install -y libgl-dev

WORKDIR /src

# Install dependencies (copy minimal files for Docker layer caching)
# claude: every .opam, since ./configure installs the deps of ./*.opam
COPY configure *.opam ./
RUN eval $(opam env) && ./configure

# Now copy the full source and build
COPY . .
RUN eval $(opam env) && make

# Test
# claude: test-lite (GOLDEN=none HEAVY=skip), as build-and-test.yml: the
# golden frames and WAVs are exact to the bit on the author's machine,
# and another's libm rounds its sines differently
RUN eval $(opam env) && make test-lite
