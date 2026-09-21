(* Scripted inputs: what the person does, frame by frame, for
 * reproducible runs of a game or an application (-script, see
 * Native_loop_2d and playground3d's Native_loop_3d), e.g. the golden
 * frame of a game after 2 seconds of play (tests/2d/Golden_frames.ml).
 *
 * A script is a comma-separated list of what:frames, the frames a
 * single frame n or a range a-b (both included), counted from 1 like
 * -dump-frame's, e.g.
 *
 *   "right:1-60,space:30,up:40-45,space:50"
 *
 * holds the right arrow for the first second, presses space at frame 30
 * (down during that frame, up at the next), jumps with up from 40 to
 * 45, and presses space again at 50. The keys are the playground's
 * names ("space", "a", ...), with left/right/up/down for the arrows.
 *
 * And the mouse, which an application needs and a game rarely does:
 *
 *   "at(-100;80):1-40,click:5,at(60;-20):6-40,click:12"
 *
 * puts the pointer at (-100, 80) -- playground coordinates, the
 * origin at the center of the screen and y up, hence the semicolon,
 * since commas separate the entries -- clicks there at frame 5, moves
 * to (60, -20) and clicks again at 12. [click] is the left button and
 * [rclick] the right one; a click at frame n is the button down
 * during n and up at n+1, which is what makes it a click
 * (Playground.mli's [mclick]).
 *
 * And characters, which are not keys (Playground.mli's [typed]):
 *
 *   "type(edit):30"
 *
 * types "edit" at frame 30, all four in that one frame's [typed], as a
 * fast typist's keystrokes arrive between two frames. No comma inside
 * the parentheses, since commas separate the entries.
 *
 * The same idea as the "input movies" of emulators (the TAS, tool-
 * assisted speedrun, communities' frame-by-frame recordings, e.g.
 * FCEUX's .fm2 files) and as the demos of Doom and Quake (.lmp and .dem
 * files: the player's inputs, replayed by a deterministic engine): with
 * a deterministic game, the inputs are the whole run. *)

type t

(* [parse s]: the script [s], or [Error msg] for a malformed one *)
val parse : string -> (t, string) result

(* [down script frame]: the keys held during [frame], by their
 * playground names, e.g. ["ArrowRight"; "space"] for frame 30 of the
 * example above *)
val down : t -> int -> string list

(* [changes script frame]: the keys going down (true) or up (false) at
 * the start of [frame], compared with the frame before, e.g. at frame
 * 31 of the example above: [("space", false)]; at frame 1: those down
 * at frame 1 *)
val changes : t -> int -> (string * bool) list

(* [mouse script frame]: where the pointer is at [frame], if the
 * script says. The last [at] covering the frame wins, so a later
 * entry can move the pointer over a stretch an earlier one covers. *)
val mouse : t -> int -> (float * float) option

(* [typed script frame]: the characters the script types at [frame],
 * "" at most frames *)
val typed : t -> int -> string

(* [button_changes script frame]: the mouse buttons going down (true)
 * or up (false) at the start of [frame], each paired with whether it
 * is the right one -- the same edges as [changes], for the two
 * buttons the playground has *)
val button_changes : t -> int -> (bool * bool) list
