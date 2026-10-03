(* Menu_update: a frame of the menu, the keys and the mouse.
 *
 * Keys: the arrows (held, they repeat), Tab and Shift-Tab (the next
 * section, across both shelves), g and a (the games' and the apps'
 * first section), Enter (play), / (search; Escape leaves it), b (the
 * grouping), p, e, m and l (the filters, each going through its values
 * and back to any), c (clear them), r (a random program of the grid),
 * s (the chosen one's code map; Escape comes back).
 *
 * The mouse: a click chooses, a double click plays (so does a click on
 * the screenshot), the wheel scrolls, the tabs, the arrows and the
 * filter bar's words are buttons, a click on the code opens it.
 *
 * While a program started from the menu runs, the menu waits for it
 * (the host's [running] and [ended]) and says how it ended. With the
 * code map open, the frame is the map's (Codemap.update). *)

open Menu_model

val update : host -> Playground.computer -> model -> model

(* the frame's time, in seconds *)
val now : Playground.computer -> Playground.number

(* what to say while the sources are not here: on their way (the web's),
 * or why not; "" when they are *)
val not_yet : sources -> string
