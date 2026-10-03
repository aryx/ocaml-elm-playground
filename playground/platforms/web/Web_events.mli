(* Web_events: the browser's events as the Playground's (Sub.event):
 * the mouse's place in the program's coordinates, the keys by the names
 * the programs know.
 *
 * The mouse. An event gives a place in the window's pixels (clientX,
 * clientY); the program wants it in its own screen's units, origin at
 * the centre, y up. Between the two: the viewBox's scaling, the empty
 * bands when the window is not the screen's shape, the scrolling. The
 * browser knows them all: the root <svg>'s getScreenCTM is the matrix
 * from its user coordinates to the window's, and its inverse the
 * conversion wanted. Always the root <svg>, never the event's target:
 * the target is the element under the pointer, a circle's small box as
 * soon as one passes there, and the place computed from it jumped.
 *)

(* [adjust_x_y svg client_x client_y]: a place in the window as the
 * program's (x, y), through the <svg>'s own matrix *)
val adjust_x_y : Js_browser.Element.t -> float -> float -> float * float

(* a browser event (keydown, keyup, the mouse's, a resize...) as the
 * Playground's, None for one it has no use for; the <svg> for the
 * mouse's place, once there is one *)
val js_event_to_event : Js_browser.Event.t -> Js_browser.Element.t option -> Sub.event option

(* a keydown's key, if it is a character typed and not a named key: the
 * browser gives the character itself for the first ("a", "A" with
 * shift, an accented letter from a dead key) and an ASCII word for the
 * second ("Shift", "ArrowUp", "F1"). So: one byte, or a first byte
 * outside ASCII. No IME *)
val typed_of_key : string -> string option

(* a line on the browser's console *)
val log : string -> unit
