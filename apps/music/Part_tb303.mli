(* The TB-303's panel and its pattern as a part (Component.mli;
 * Part_hammond.mli says why a part, and how it closes over its voice).
 *
 * The knobs, the 303's in its order (Tuning, Cut Off Freq, Resonance,
 * Env Mod, Decay, Accent), then the waveform, the tempo and the volume;
 * RUN. Under them the pattern as a grid to see and click, which the real
 * 303's keypad never showed: the 16 steps as columns, a piano roll of an
 * octave above C2 -- a click sets the step's note, again the same cell
 * makes it a rest -- and under it each step's octave, accent and slide.
 * A click on a step's number holds it: the sound's knobs then show and
 * set that step's parameter locks (CLEAR: none; the rocker says what
 * they mean between steps, SMOOTH how they glide). A preset chosen
 * (the part's menu) lets go of the step held. *)

val kind : string

(* 960 x 600: the knobs' strip and the grid *)
val natural : float * float
val make : Voice_tb303.t -> Component.part
val load : Voice_tb303.t -> string -> Component.part
