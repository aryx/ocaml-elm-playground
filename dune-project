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

(version "0.2.1")

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
    (elm_core (= :version))
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
    (elm_core (= :version))
    (elm_system (= :version))
  )
)
(package (name elm_playground_native)
  (synopsis "Native TDSL-based playground platform")
  (description "")
  (depends
    ; the lib fails in 4.07 because of some syntax error about an operator
    ; used to also exclude >= 5.2.0 because tsdl failed there with some
    ; missing unix error, but that got fixed upstream (probably in
    ; ctypes/ctypes-foreign, not tsdl itself): verified 2026-09-16 that
    ; OCaml 5.2.0 through 5.5.1 all build and pass the tests fine now.
    (ocaml (>= "4.08.0"))
    logs
    (cairo2 (>= "0.6.4"))
    tsdl
    ; 0.1's `result` type is [ `Ok of 'a | `Error of string ], incompatible
    ; with the Ok/Error (`Msg _) pattern used since; 0.3 introduced the
    ; current API but Stb_image.image (used to compose GIF frames) was only
    ; added in 0.4.
    (stb_image (>= "0.4"))
    (elm_playground (= :version))
    (elm_playground_native_common (= :version))
  )
)
(package (name elm_playground_native_common)
  (synopsis "SDL event loop + image loading shared by native 2D playground backends")
  (description "\
  Not meant to be used directly -- the Cairo-free SDL window/event loop
  (Native_loop_2d) shared by the native 2D playground backends, and,
  as a private library, the image download/decode/GIF-animation code of
  graphics/images/ (Image_decode). A plain library rather than an
  (implements elm_playground): two implementations of the same virtual
  library can't depend on each other.
")
  (depends
    ; see elm_playground_native above for these version constraints
    (ocaml (>= "4.08.0"))
    logs
    tsdl
    (stb_image (>= "0.4"))
    ocurl
    (elm_system (= :version))
  )
)
(package (name elm_playground_software)
  (synopsis "Native playground platform with a from-scratch software rasterizer")
  (description "\
  Like elm_playground_native, but every pixel is computed by our own
  code instead of Cairo; SDL only opens the window, reads input, and
  shows the pixels. The algorithms (framebuffer, affine transforms,
  polygon filling, Bresenham lines, midpoint circles, image warping,
  compositing, antialiasing, a stroke font), written to be read, with
  references to the papers that introduced them, are in graphics/,
  private libraries installed as part of this package for now; see
  docs/claude_notes/notes_2d.md.
")
  (depends
    (ocaml (>= "4.08.0"))
    tsdl
    (stb_image (>= "0.4"))
    (elm_core (= :version))
    (elm_playground (= :version))
    (elm_playground_native_common (= :version))
    (testo :with-test)
    (alcotest :with-test)
    (cairo2 :with-test) ; to compare our pixels with Cairo's
  )
)
(package (name elm_playground_web)
  (synopsis "Web vdom-based playground platform")
  (description "")
  (depends
     ; gen_js_api fails to build in 4.10.0
     (ocaml (>= "4.11.0"))
     vdom
     (elm_playground (= :version))
  )
)

(package (name elm_playground_3d)
  (synopsis "Interface of the 3D Playground")
  (description "\
  This is just the interface of the 3D playground. You'll need an
  actual platform library to compile your application
  (elm_playground_3d_software or elm_playground_3d_web)
")
  (depends
    (elm_core (= :version))
    (elm_playground (= :version))
  )
)
(package (name elm_playground_3d_native_common)
  (synopsis "SDL event loop shared by native-ish 3D playground backends")
  (description "\
  Not meant to be used directly -- a small library factored out of
  elm_playground_3d_software so the OpenGL backend (see
  docs/claude_notes/plan_opengl.md) can share the same SDL
  window-event-draining/frame-pacing loop instead of duplicating it
  (textures are loaded by graphics/images/'s Texture_decode). The
  event loop (Native_loop) is generic over the model/view types (not a
  dependency of elm_playground_3d itself) so that depending on it
  doesn't create a second path to that virtual library alongside a
  backend's own (implements elm_playground_3d) -- dune forbids that.
")
  (depends
    (ocaml (>= "4.08.0"))
    tsdl
    logs ; parse_cli_and_setup_logging, and run's per-frame Logs.debug fps line
    (elm_playground (= :version))
  )
)
(package (name elm_playground_3d_software)
  (synopsis "Native from-scratch software rasterizer for the 3D playground")
  (description "")
  ; claude: NO cairo2 (nor elm_playground_native) anymore: every pixel,
  ; the HUD's 2D shapes and text included, is computed by our own code
  ; (graphics/3d/, graphics/2d/ via Shape_render_software); SDL only
  ; opens the window and shows the pixels. See
  ; docs/claude_notes/done/plan_code_reorg_teaching_3d.md, phase 5.
  (depends
    (ocaml (>= "4.08.0"))
    tsdl
    (stb_image (>= "0.4")) ; Stb_image.t, what Texture_decode gives
    (elm_playground_3d (= :version))
    (elm_playground_3d_native_common (= :version))
    (elm_playground_native_common (= :version)) ; graphics/images (Texture_decode)
    (elm_playground_software (= :version)) ; graphics/core and graphics/2d, used by graphics/3d; Shape_render_software, for the HUD pass
  )
)
(package (name elm_playground_3d_web)
  (synopsis "Web platform for the 3D playground, reusing elm_playground_web")
  (description "")
  (depends
    (ocaml (>= "4.11.0"))
    (elm_playground_3d (= :version))
    (elm_playground_web (= :version))
  )
)
(package (name elm_playground_3d_opengl)
  (synopsis "Real GPU-accelerated (OpenGL 3.3 core) platform for the 3D playground")
  (description "\
  See docs/claude_notes/plan_opengl.md -- a third playground3d backend,
  alongside the native software rasterizer and the web SVG one, that
  hands the same shape3d/camera scenes to a real GPU via OpenGL
  instead of hand-rolling the rasterization/depth-test/shading math.
")
  (depends
    (ocaml (>= "4.08.0"))
    tsdl
    tgls
    (stb_image (>= "0.4")) ; Stb_image.t, used by the type of get_or_create_gl_texture
    (elm_playground_3d (= :version))
    (elm_playground_3d_native_common (= :version))
    (elm_playground_native_common (= :version)) ; graphics/images (Texture_decode)
  )
)
