(* The mouse's cursor on the SDL platforms (Playground_platform's
 * [set_cursor], which tells what each is for): one of the system's
 * own shapes, made the first time it is asked for, or none. Its own
 * type, as this library cannot know Playground's (the dune file says
 * why). Asking for the cursor already shown costs nothing: a program
 * may ask at every frame. *)

type t = [ `Arrow | `Hand | `Text | `Crosshair | `Hidden ]

val set : t -> unit
