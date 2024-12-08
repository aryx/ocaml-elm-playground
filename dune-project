(lang dune 3.7)
(name ocaml_elm)

;; Opam package declarations for public libraries and public executables
;; defined in various dune files.
;; See the documentation for declaring opam packages for more info:
;; https://dune.readthedocs.io/en/stable/opam.html#opam-generation
(generate_opam_files)

;; Default attributes of opam packages
(source (github aryx/ocaml-elm-playground))
;(homepage "https://")
(maintainers "Yoann Padioleau")
(authors "Yoann Padioleau, Evan Czaplicki")

(package (name elm_core)
  (synopsis "Stuff")
  (description "\
Nope
"
  )

  (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (authors "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (license "MIT")
  (depends
;    base
;    (cmdliner (>= 1.1.0))
;    ocaml-protoc-plugin
;    grpc
;    grpc-eio
;    eio_main
;    (eio (>= 1.0))
  )
)

(package (name elm_system)
  (synopsis "XXX")
  (description "XXX")
)

(package (name elm_playground)
  (synopsis "XXX")
  (description "XXX")
)
(package (name elm_playground_native)
  (synopsis "XXX")
  (description "XXX")
)
(package (name elm_playground_web)
  (synopsis "XXX")
  (description "XXX")
)
