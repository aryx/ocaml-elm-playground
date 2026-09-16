# Build and test the OCaml Elm Playground with OCaml 4.14.4 via OPAM on Ubuntu.
# See also .github/workflows/docker.yml for its use in Github Actions (GHA).
#
# Note: elm_playground_native.opam pins ocaml < 5.2.0 (tsdl fails to build
# past that), confirmed by trying --build-arg OCAML_VERSION=5.2.1 (opam
# solver rejects it outright). 5.1.1 is the newest OCaml 5 release that
# still satisfies that bound - see 'make build-docker-ocaml5'.

FROM ubuntu:22.04

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

WORKDIR /src

# Install dependencies (copy minimal files for Docker layer caching)
COPY configure elm_core.opam elm_system.opam elm_playground.opam elm_playground_native.opam elm_playground_web.opam ./
RUN eval $(opam env) && ./configure

# Now copy the full source and build
COPY . .
RUN eval $(opam env) && make

# Test
RUN eval $(opam env) && make test
