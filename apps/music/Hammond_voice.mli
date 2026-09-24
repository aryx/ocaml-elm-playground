(* The Hammond organ's voice: additive synthesis, nine sines per key
 * from the tonewheels (Tonewheel.mli), the percussion, the key click,
 * the scanner vibrato (plan_synth_teaching.md, TinyHammond, H2).
 *
 * {1 The drawbars: additive synthesis}
 *
 * The subtractive synthesizer (the Minimoog) starts rich and takes
 * away; the organ builds up: a sound is sines added, each at its own
 * level. A Hammond key connects nine wheels, one per *drawbar*, each
 * pulled out 0 (silent) to 8 (loudest), named after an organ pipe's
 * length -- 8' is the note itself:
 *
 *     drawbar   16'   5 1/3'  8'   4'   2 2/3'  2'   1 3/5'  1 1/3'  1'
 *     harmonic  1/2   3/2     1    2    3       4    5       6       8
 *     semitones -12   +7      0    +12  +19     +24  +28     +31     +36
 *
 * and a registration is the nine positions: 888000000 (the first three
 * out: the jazz organ), 008000000 (the note alone: a flute). The
 * harmonics are the *wheels* of those notes, tempered, not the true
 * harmonics (Tonewheel.mli: the 2 2/3' of C4 is 1.36 cents flat), and
 * a note past the wheels there are folds back an octave (the lowest
 * keys' 16', the highest keys' 1'). Each step of a drawbar is 3 dB (ours:
 * the common description), 8 at full level. The wheels never stop, a
 * key only connects them: the voices read one shared clock, and two
 * keys sharing a wheel add in phase, as on the organ.
 *
 * No envelope: a key's contacts close, the sound is there, 2 ms of ramp
 * (ours) against a click from the jump itself -- and a click on
 * purpose, the *key click* (the contacts bouncing, Hammond's engineers'
 * defect, its players' taste): a few ms of noise at each key down.
 *
 * {1 The percussion}
 *
 * The B-3's one envelope (1955): a second or third harmonic (the 4' or
 * 2 2/3' wheel) struck at the key and dying away, fast or slow, normal
 * or soft -- a pluck on the attack, the jazz organ's. *Single-trigger*:
 * only a key pressed when no other is held gets it, so a legato line
 * keeps it on its first note; and while it is on, the 1' drawbar is
 * silent (the percussion takes its circuit). Its times ours: fast to
 * -60 dB in 0.3 s, slow in 1.2 s; soft 6 dB under normal.
 *
 * {1 The scanner vibrato}
 *
 * A delay line of coils and capacitors, its taps swept by a rotating
 * capacitor (the scanner) 6.9 times a second (ours: the motor's rate as
 * commonly given): the sound read at a moving delay, a vibrato (V1,
 * V2, V3, deeper each), or that mixed with the dry sound, a chorus
 * (C1, C2, C3). Here a delay line read at a delay a triangle moves,
 * 0.2, 0.45 and 0.8 ms either way (ours: to be measured against a
 * recording).
 *
 * Worked example (Unit_hammond): C4 on 888000000, its spectrum three
 * lines of the same height (within 1%) at the 16', 5 1/3' and 8'
 * wheels (130.8, 196.0, 261.5 Hz), nothing at 4' (523 Hz, under -60
 * dB); 006000000 against 008000000, -6 dB; with the third harmonic's
 * percussion, G5's wheel (784 Hz) there at the attack and more than 60
 * dB down a second later; E4 pressed while C4 is held, its own third
 * harmonic never struck; a chord's three voices freed 2 ms after
 * they're let go. *)

type patch = {
  drawbars : int array; (* 9, each 0 to 8, 16' first *)
  percussion : bool;
  third : bool; (* the percussion's harmonic: the third (2 2/3'), else the second (4') *)
  fast : bool;
  soft : bool;
  click : float; (* 0 to 1 *)
  vibrato : int; (* an index in [vibratos] *)
  volume : float; (* 0 to 1 *)
}

(* "off", "V1", "V2", "V3", "C1", "C2", "C3" *)
val vibratos : string list

(* the drawbars' footages, "16'" to "1'", and their semitones above the
 * key, -12 to +36 *)
val footages : string list
val semitones : int list

(* [registration "888000000" p]: [p] with those drawbars; [of_registration p]
 * the nine digits *)
val registration : string -> patch -> patch
val of_registration : patch -> string

(* 888000000, no percussion, some click, no vibrato *)
val initial : patch

(* a control, by name: "drawbar.16", "drawbar.5-1/3", ..., "drawbar.1",
 * "percussion", "percussion.third", "percussion.fast",
 * "percussion.soft", "click", "vibrato", "volume" *)
type knob = patch Patch_text.knob

val knobs : knob list
val to_string : patch -> string
val of_string : string -> (patch, string) result

(* our registrations, after the classics: jazz (888000000, percussion
 * third, fast, soft), full (888888888), gospel (888808008, C3), ballad
 * (838000000, C3), flute (008000000, V2) *)
val presets : (string * patch) list

(* [drawbar_gain level]: 0 silent, then 3 dB a step, 8 at 1 *)
val drawbar_gain : int -> float

type t

val create : patch -> t
val patch : t -> patch
val set_patch : t -> patch -> unit

(* the voices sounding (Polyphony.voices) *)
val voices : t -> int

(* the organ as an instrument (Instrument.mli): polyphonic, velocity
 * ignored (an organ's keys are switches), [set] by a control's name *)
val instrument : t -> Instrument.t
