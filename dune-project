(lang dune 2.7)
(name ocaml_elm)

;; Opam package declarations for public libraries and public executables
;; defined in various dune files.
;; See the documentation for declaring opam packages for more info:
;; https://dune.readthedocs.io/en/stable/opam.html#opam-generation
; (generate_opam_files)

;; Default attributes of opam packages
; (source (github semgrep/semgrep-proprietary))
; (homepage "https://semgrep.dev")
; (maintainers "Semgrep authors")
; (authors "Semgrep authors")

; (package (name semgrep-server)
;   (synopsis "Like semgrep but in a server")
;   (description "\
; Nope
; "
;   )
; 
;   (maintainers "Yoann Padioleau <pad@semgrep.com>")
;   (authors "Yoann Padioleau <pad@semgrep.com>")
;   ;(license "Commercial")
;   (depends
;     base
;     (cmdliner (>= 1.1.0))
;     ocaml-protoc-plugin
;     grpc
;     grpc-eio
;     eio_main
;     (eio (>= 1.0))
;   )
; )

