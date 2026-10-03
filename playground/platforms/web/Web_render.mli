(* Web_render: a frame's shapes as SVG, elm-playground's own way: each
 * Playground shape an SVG element (a circle a <circle>, words a <text>,
 * a group a <g>), its place, angle and scale one transform attribute,
 * the whole in an <svg> whose viewBox is the program's screen, so that
 * the browser scales and letterboxes it to the window.
 *
 * The y axis: the Playground's goes up, SVG's down, so every y is
 * negated here (and back in Web_events.adjust_x_y, for the mouse).
 *
 * The result is a description (Web_vdom.t), built anew each frame and
 * patched into the page by Playground_platform.run_app.
 *
 * A bitmap (Playground.bitmap, pixels made by the program) is an
 * <image> whose href is a PNG data: URL. Encoding one is the costly
 * part, so the URLs are kept, by the image's identity (==), up to 16
 * million pixels' worth; the PNG is encoded by the browser itself, the
 * pixels put on a canvas and the canvas asked for it: 28 ms where our
 * own encoder compiled to JavaScript took a second.
 *)

(* the <svg> of a frame: full window, the viewBox the screen's;
 * [rendering]'s switches as the browser's own (shape-rendering,
 * image-rendering), inherited by every shape inside *)
val render : rendering:Playground.rendering -> Playground.screen -> Playground.shape list -> 'msg Web_vdom.Svg.t
