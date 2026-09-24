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

;; claude: two interfaces, and the platforms implementing them. A program
;; links one interface's platform; a 3D program links a 2D platform too
;; (Playground3d is built on Playground, and draws its HUD with it):
;;
;;                 2D (elm_playground)        3D (elm_playground_3d)
;;   native        elm_playground_native      elm_playground_3d_opengl
;;                 (Cairo)                    + elm_playground_software
;;   web           elm_playground_web         elm_playground_3d_webgl
;;                 (SVG)                      + elm_playground_web
;;   teaching      elm_playground_software    elm_playground_3d_software
;;   (from scratch)                           + elm_playground_software
;;   fallback                                 elm_playground_3d_web (SVG)
;;                                            + elm_playground_web
;;
;; The libraries the platforms share but nobody else should use are
;; private (no .opam of their own), see playground/platforms/native_common/dune.

(package (name elm_playground)
  (synopsis "Interface of the Playground: pictures, animations and games")
  (description "\
  This is just the interface of the 2D playground, and the libraries
  built on it (physics, audio, widgets, ...). You'll need an actual
  platform library to compile your application: elm_playground_native
  for the desktop, or elm_playground_web for the browser. For 3D, see
  elm_playground_3d.
")
  (depends
    ; ocaml-ci reports "Unbound module Float" before (in core/)
    (ocaml (>= "4.07.0"))
    ; claude: capabilities, for Playground_platform's store/fetch/export
    ; (docs/claude_notes/plans/plan_caps.md); links under js_of_ocaml too
    (caps (>= "0.1.0"))
  )
)
(package (name elm_playground_native)
  (synopsis "Native playground platform: SDL window, Cairo drawing")
  (description "\
  The desktop platform for a 2D program: SDL opens the window and reads
  the input, Cairo draws the shapes. (A 3D program uses
  elm_playground_software as its 2D platform instead, see
  elm_playground_3d.)
")
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
    (elm_playground (= :version))
    ; claude: for playground/platforms/native_common/ and graphics/images/, private
    ; libraries installed as part of it (see native_common/dune for why)
    (elm_playground_software (= :version))
  )
)
(package (name elm_playground_software)
  (synopsis "Native playground platform with a from-scratch software rasterizer")
  (description "\
  Like elm_playground_native, but every pixel is computed by our own
  code instead of Cairo; SDL only opens the window, reads input, and
  shows the pixels. Meant for teaching: the algorithms (framebuffer,
  affine transforms, polygon filling, Bresenham lines, midpoint
  circles, image warping, compositing, antialiasing, a stroke font) are
  written to be read, with references to the papers that introduced
  them. It is also the 2D platform of the native 3D programs, whose HUD
  it draws, and installs the SDL event loops and the image loading
  shared by all the native platforms.
")
  ; claude: the algorithms are graphics/'s private libraries, installed
  ; as part of this package; see notes_2d.md
  (depends
    (ocaml (>= "4.08.0"))
    logs
    tsdl
    ocurl
    (elm_playground (= :version))
    (testo :with-test)
    (alcotest :with-test)
    (cairo2 :with-test) ; to compare our pixels with Cairo's
  )
)
(package (name elm_playground_web)
  (synopsis "Web playground platform: js_of_ocaml, vdom and SVG")
  (description "\
  The browser platform, for a 2D program and a 3D one alike: the same
  application code as on the desktop, compiled by js_of_ocaml, its
  shapes drawn as SVG through vdom.
")
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
  actual platform library to compile your application, and a 2D one
  with it (the 3D playground is built on the 2D one, which draws its
  HUD): elm_playground_3d_opengl with elm_playground_software for the
  desktop, elm_playground_3d_webgl with elm_playground_web for the
  browser. elm_playground_3d_software draws every pixel with our own
  code, for teaching; elm_playground_3d_web draws small scenes as SVG,
  without a GPU.
")
  (depends
    (elm_playground (= :version))
  )
)
(package (name elm_playground_3d_software)
  (synopsis "Native 3D playground platform with a from-scratch software rasterizer")
  (description "\
  Like elm_playground_3d_opengl, but every pixel is computed by our own
  code instead of the GPU's: transforms, clipping, triangle filling,
  the z-buffer, shading and texturing, written to be read. Meant for
  teaching; slower than the GPU.
")
  ; claude: NO cairo2 (nor elm_playground_native): every pixel, the
  ; HUD's 2D shapes and text included, is computed by our own code
  ; (graphics/3d/, graphics/2d/ via Shape_render_software); SDL only
  ; opens the window and shows the pixels. See notes_3d.md.
  (depends
    (ocaml (>= "4.08.0"))
    tsdl
    (elm_playground_3d (= :version))
    (elm_playground_software (= :version)) ; graphics/core and graphics/2d, used by graphics/3d; Shape_render_software, for the HUD pass; the SDL loop; graphics/images (Texture_decode)
  )
)
(package (name elm_playground_3d_web)
  (synopsis "Web 3D playground platform drawing through SVG")
  (description "\
  The scene compiled every frame down to plain 2D shapes (backface
  culling, the painter's algorithm), drawn by elm_playground_web: no
  GPU needed, but for small scenes. elm_playground_3d_webgl is the web
  platform to pick otherwise.
")
  (depends
    (ocaml (>= "4.11.0"))
    (elm_playground_3d (= :version))
    (elm_playground_web (= :version))
  )
)
(package (name elm_playground_3d_opengl)
  (synopsis "Native 3D playground platform on the GPU (OpenGL 3.3 core)")
  (description "\
  The desktop platform for a 3D program: the scenes handed to the GPU
  through OpenGL, the HUD drawn by elm_playground_software, its 2D
  platform.
")
  ; claude: see plan_opengl.md and notes_opengl.md
  (depends
    (ocaml (>= "4.08.0"))
    tsdl
    tgls
    (elm_playground_3d (= :version))
    (elm_playground_software (= :version)) ; the HUD: Shape_render_software, graphics/core (Matting); the SDL loop; graphics/images (Texture_decode)
  )
)
(package (name elm_playground_3d_webgl)
  (synopsis "Web 3D playground platform on the GPU (WebGL 1)")
  (description "\
  The browser platform for a 3D program: the scenes drawn by the GPU,
  reusing elm_playground_web (its 2D platform) for the event loop and,
  as SVG, the HUD.
")
  ; claude: see plan_webgl.md
  (depends
    (ocaml (>= "4.11.0"))
    ; claude: 6.0 for Js_of_ocaml.Console (Firebug is deprecated since)
    ; and for the types the backend assumes: a canvas' naturalWidth an
    ; int rather than an optdef, and Js.number_t/WebGL.clampf an
    ; abstract Javascript number rather than an OCaml float.
    (js_of_ocaml (>= "6.0"))
    (js_of_ocaml-ppx (>= "6.0"))
    (elm_playground_3d (= :version))
    (elm_playground_web (= :version))
  )
)
