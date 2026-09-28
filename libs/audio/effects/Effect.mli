(* An effect as a rack sees it: a sound going through, block after
 * block, and knobs turned by their names (see notes_synth.md section 8).
 *
 * Each effect here has its own typed interface, what its .mli explains
 * and its tests use (Delay.process t settings: a record of the delay's
 * numbers). A rack wants something else: to hold effects of every kind
 * in a list, in an order it can change, and to turn their knobs from a
 * patch, which stores numbers by name. So each module also makes an
 * [Effect.t], its settings kept inside:
 *
 *     Delay.effect () : Effect.t
 *
 *        knobs    time, feedback, tone, pingpong, mix: what a panel
 *                 draws and a patch stores (Control.mli)
 *        set      "time" 0.5: a knob turned, heard from the next block
 *        process  a stereo block, in place
 *        meters   what it shows back: a compressor's gain reduction,
 *                 the needle on its front panel
 *
 * the same shape as Instrument.t, for the same reason: a record of
 * functions (an object in all but name) over a state the closures
 * share, the kind the rack holds not knowing what's inside -- the
 * plug-in's interface (VST's, 1996: parameters by index, a
 * processReplacing of blocks) made small.
 *
 * A knob turned between two blocks is *ramped* over the next one, as
 * Instrument.mli's are, wherever it multiplies the sound (a gain, a
 * mix): a jump there is a step in the wave, a click, and a knob turned
 * slowly a click every block, zipper noise. [ramp] is the ramp. *)

(* a knob: its name, its control, its position at first *)
type knob = { name : string; control : Control.t; initial : float }

type t = {
  name : string; (* "delay" *)
  knobs : knob list;
  (* a knob turned, by its name; names it doesn't have are ignored *)
  set : string -> float -> unit;
  (* a block through it, in place, both channels *)
  process : Signal.stereo -> unit;
  (* its meters, by name, as of the last block ([] for most) *)
  meters : unit -> (string * float) list;
}

(* [ramp last now i n]: sample [i] of a block of [n], a knob going from
 * [last] (the last block's) to [now]: last + (now - last) (i + 1) / n,
 * [now] at the block's end *)
val ramp : float -> float -> int -> int -> float
