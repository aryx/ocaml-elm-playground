(lang dune 3.0)

(name elm_playground)

(generate_opam_files true)

(license "LGPL-2.1-only")

(authors
   "Yoann Padioleau <yoann.padioleau@gmail.com"
)

(maintainers 
   "Yoann Padioleau <yoann.padioleau@gmail.com"
)

(version "0.1.5")

(source (github aryx/ocaml-elm-playground))
(homepage "https://github.com/aryx/ocaml-elm-playground")
(bug_reports "https://github.com/aryx/ocaml-elm-playground/issues")
;TODO: dune-release want a aryx.github.io URL
;(documentation "https://github.com/aryx/ocaml-elm-playground/")

;; Opam package declarations for public libraries and public executables
;; defined in various dune files.
;; See the documentation for declaring opam packages for more info:
;; https://dune.readthedocs.io/en/stable/opam.html#opam-generation

(package (name elm_core)
  (synopsis "Imitate core Elm functions for ease of porting")
  (description "\
Nope
"
  )
  (depends
    ; ocaml-ci reports "Unbound module Float" before
    (ocaml (>= "4.07.0"))
  )
)

(package (name elm_system)
  (synopsis "Imitate system Elm functions for ease of porting")
  (description "")
  (depends
    elm_core
  )
)

(package (name elm_playground)
  (synopsis "Interface of the Playground")
  (description "\
  This is just the interface of the playground. You'll need an
  actual platform library to compile your application
  (elm_playground_native or elm_playground_web)
")
  (depends
    elm_core
    elm_system
  )
)
(package (name elm_playground_native)
  (synopsis "Native TDSL-based playground platform")
  (description "")
  (depends
    ; tsdl fails in 5.2.0 because of some missing unix
    ; the lib fails in 4.07 because of some syntax error about an operator
    (ocaml (and (< "5.2.0") (>= "4.08.0")))
    logs
    (cairo2 (>= "0.6.4"))
    tsdl
    imagelib
    ocurl
    elm_playground
  )
)
(package (name elm_playground_web)
  (synopsis "Web vdom-based playground platform")
  (description "")
  (depends
     ; gen_js_api fails to build in 4.10.0
     (ocaml (>= "4.11.0"))
     vdom
     elm_playground
  )
)
