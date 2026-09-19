(* Scripted inputs: game keys held down over given frames, for
 * reproducible runs of a game (-script, see Native_loop_2d and
 * playground3d's Native_loop_3d), e.g. the golden frame of a game after 2
 * seconds of play (tests/2d/Golden_frames.ml).
 *
 * A script is a comma-separated list of key:frames, the frames a single
 * frame n or a range a-b (both included), counted from 1 like
 * -dump-frame's, e.g.
 *
 *   "right:1-60,space:30,up:40-45,space:50"
 *
 * holds the right arrow for the first second, presses space at frame 30
 * (down during that frame, up at the next), jumps with up from 40 to
 * 45, and presses space again at 50. The keys are the playground's
 * names ("space", "a", ...), with left/right/up/down for the arrows.
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
