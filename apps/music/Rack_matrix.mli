(* Reason's Matrix Pattern Sequencer as a rack device (Rack_device.mli):
 * no sound of its own, three CV outputs -- Note (0), Gate (1), Curve
 * (2) -- cabled into an instrument's Seq Note and Seq Gate (and the
 * curve into any knob's CV input).
 *
 * 16 steps, each a note, a gate (its height the velocity, 0 a rest), a
 * tie (the gate held into the next step: a slide, for a monophonic
 * voice) and a curve value. A step lasts a sixteenth; its gate is open
 * for the first half of it, or all of it when tied.
 *
 *   step     1    2    3    4
 *   gate   ##__ ##__ ######## ##__      (3 tied into 4)
 *
 * Its knobs, by name: "step1.note" .. "step16.note" (0 to 127),
 * "stepK.gate" (0 to 1), "stepK.tie" (0 or 1), "stepK.curve" (0 to 1).
 *
 * Ours, and said so: 16 steps (Reason's has up to 32 and pattern
 * banks); the CVs change at chunks, up to 63 samples (1.4 ms) from the
 * step's exact sample. *)

val steps : int (* 16 *)
val create : unit -> Rack_device.t

val note_out : int
val gate_out : int
val curve_out : int

(* [position ~tempo pos]: [pos] samples after the start, the step
 * sounding (0 to 15) and how far into it, 0 to 1 *)
val position : tempo:float -> float -> int * float
