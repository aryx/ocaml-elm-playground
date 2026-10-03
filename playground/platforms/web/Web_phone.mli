(* Web_phone: a phone or a tablet (plan_mobile.md, notes_mobile.md),
 * kept apart from the desktop's simpler path: Playground_platform.run_app
 * calls the three functions below, and nothing else of it knows about
 * fingers. A program is not told either: a finger is its mouse, the
 * keys on the screen its keyboard.
 *
 *   - the page: laid out at the phone's own width, no double-tap zoom,
 *     a drag the program's and not the page's scroll;
 *   - fingers: pointer events as the mouse's. A tap is the button down
 *     and up at the finger, a drag its moves between, two taps close in
 *     time and place a double click;
 *   - keys: made at the first touch, so never with a mouse alone. A
 *     button in the corner shows a row of the keys games use (the
 *     arrows, space...); its "abc" brings up the phone's own keyboard,
 *     through a text field that cannot be seen, what it types becoming
 *     keys, each down then up a moment later, and the typed text;
 *   - the drawing: then at the top of the screen, not its middle, and
 *     as high as what the keyboard and the key row leave.
 *)

(* the viewport tag and the touch styles, added to the page so that no
 * page needs them *)
val phone_page : unit -> unit

(* the phone's keys: [phone_keys ~process ~svg] is a function giving
 * their element, made the first time it is called (the first touch),
 * the drawing ([svg], once there is one) then laid out for a phone.
 * [process]: run_app's, where the keys' events go *)
val phone_keys : process:(Sub.event -> unit) -> svg:(unit -> Js_browser.Element.t option) -> unit -> Ojs.t

(* the window listened to for pointer events, sent to [process] as the
 * mouse's; [svg] for the place (Web_events.adjust_x_y); [keys],
 * phone_keys', whose taps are its own and not the program's *)
val listen_to_fingers : svg:(unit -> Js_browser.Element.t option) -> process:(Sub.event -> unit) -> keys:(unit -> Ojs.t) -> unit
