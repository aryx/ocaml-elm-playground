(* Voicing: which of the keys held a voice plays, and how it gets from
 * one note to the next (see notes_synth.md section 5).
 *
 * A monophonic synthesizer (the Minimoog, the TB-303) has one voice and
 * ten fingers to listen to. It keeps the keys held, in the order they
 * were pressed, and a rule, the *priority*, picks the one that sounds:
 *
 *     hold C4, then press E4:     Low: C4   High: E4   Last: E4
 *     let go of E4:               C4 for all three (the only one left)
 *     hold C4 and E4, press D4:   Low: C4   High: E4   Last: D4
 *
 * Low-note priority is the Minimoog's (its keyboard circuit reads the
 * lowest key down); last-note the choice of most monosynths since, the
 * one that feels natural when playing fast. Keeping the keys held,
 * not only the last one, is what makes a trill possible: hold one key,
 * tap another, and each release goes back to the held note.
 *
 * What the voice has to do then is an [event]: a note begins from
 * silence (the envelopes' gate opens), the note *changes* while the
 * gate stays open, or the last key is let go. Changing the note without
 * reopening the gate is *legato*: the envelopes go on where they are,
 * one phrase; with [~retrigger:true] every change reopens it (the
 * attack again, from the level where it is: Envelope.mli), each note
 * articulated. The Minimoog is legato ("single trigger").
 *
 * {1 Glide}
 *
 * The pitch going to the new note instead of jumping (portamento), a
 * one-pole on the pitch in *semitones*, not in hertz:
 *
 *     p += (target - p) c,    c = 1 - e^(-1 / (tau rate))
 *
 * so an octave up and an octave down take the same time, as the ear
 * expects (in hertz, the octave down would cover half as many hertz).
 * The glide knob is the time constant tau: after tau, 63.2% of the way
 * (1 - 1/e); after 5 tau, 99.3%. Worked example: C4 to C5, 12
 * semitones, tau = 0.1 s: after 0.1 s at 67.59 (between G4 and G#4,
 * 405.5 Hz), after 0.5 s at 71.92, 8 cents under C5, 520.8 Hz. A glide
 * of 0 jumps. The pitch keeps its value when the keys are let go, so
 * the next note glides from the last one, as the Minimoog's did (its
 * pitch a voltage held on a capacitor).
 *
 * References: the Minimoog Model D's keyboard (low-note priority,
 * single trigger); Will Pirkle, Designing Software Synthesizer Plug-Ins
 * in C++, 2014, chapter 8 (voice management, portamento). *)

type priority = Low | High | Last

(* what the voice does after a key: begin the note [n] with its gate
 * opening; change to [n], the gate staying as it is; close the gate;
 * nothing *)
type event = Begin of int | Change of int | End | Nothing

type t

(* [create ?priority ?retrigger ()]: no key held; [Last] and legato by
 * default *)
val create : ?priority:priority -> ?retrigger:bool -> unit -> t

(* [press t key], [release t key]: a key (MIDI's numbers) down, up *)
val press : t -> int -> event
val release : t -> int -> event

(* the key sounding, if any *)
val sounding : t -> int option

(* {1 Glide} *)

type glide

(* at [note] (a MIDI number, 60 by default) *)
val glide : ?note:float -> unit -> glide

(* [glide_to g note]: the pitch now moving to [note] *)
val glide_to : glide -> int -> unit

(* [fill_frequency g ~seconds out]: the next [Array.length out]
 * frequencies, in Hz, the pitch gliding with the time constant
 * [seconds] (0: at once) *)
val fill_frequency : glide -> seconds:float -> Signal.t -> unit

(* [fill_pitch]: the same, the pitch in semitones (a MIDI number,
 * fractional), for a voice adding more to it -- a wheel, a vibrato --
 * before turning it into hertz *)
val fill_pitch : glide -> seconds:float -> Signal.t -> unit

(* the pitch now, in semitones (a MIDI number, fractional) *)
val pitch : glide -> float

(* [frequency pitch]: 440 x 2^((pitch - 69) / 12), for a fractional
 * pitch *)
val frequency : float -> float
