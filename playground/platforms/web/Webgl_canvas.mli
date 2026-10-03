(* Webgl_canvas: the <canvas> WebGL draws in, on a page that the 2D web
 * platform owns: its run_app puts an <svg> there for the HUD, and the
 * canvas goes under it.
 *
 * Three things a page makes one handle. The canvas must stay in the
 * page (run_app empties <body> before its first frame). It has two
 * sizes, the one on the page in CSS pixels and its drawing buffer's in
 * real pixels, kept equal to the first times devicePixelRatio, or the
 * browser stretches the picture. And the program's screen is a shape of
 * its own, to fit in the window with margins, the same way the <svg>
 * fits its viewBox, so that the scene and the HUD line up.
 *)

open Js_of_ocaml

(* a canvas the size of the window, fixed, painted under the <svg> *)
val create_canvas : unit -> Dom_html.canvasElement Js.t

(* the element put back in <body> if it is not there (a no-op on every
 * frame but the one after run_app emptied the page) *)
val ensure_in_page : #Dom.node Js.t -> unit

(* what the page says, instead of staying blank, when the browser has
 * no WebGL *)
val no_webgl_message : Dom_html.paragraphElement Js.t Lazy.t

(* the drawing buffer given the page's size times devicePixelRatio,
 * each frame, to follow the window; its width and height in real
 * pixels *)
val resize_to_window : Dom_html.canvasElement Js.t -> int * int

(* where the screen goes in a canvas, (x, y, width, height) in its
 * pixels: the largest scale at which it fits, centred, the margins
 * left and right or top and bottom
 *
 *    canvas_w
 *   +---------+-------------------+---------+
 *   |         |                   |         |
 *   | x       |  screen.width *   |         | canvas_h
 *   |<------->|  scale            |         |
 *   |         |                   |         |
 *   +---------+-------------------+---------+
 *)
val letterbox : canvas_w:int -> canvas_h:int -> Playground.screen -> int * int * int * int
