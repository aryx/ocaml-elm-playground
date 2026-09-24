(* ReBirth: two TB-303s, a TR-808 and a TR-909 on one clock, mixed and
 * through effects (see notes_synth.md; plan_synth_teaching.md,
 * TinyReBirth).
 *
 * A *studio*, not a voice: where a Voice_xxx is one instrument
 * (Voice.mli), a Studio_xxx gathers several with what goes between
 * them -- a clock, a mixer, effects -- the OP-1 and the OP-XY to come
 * (Studio_op1, Studio_opxy).
 *
 * Propellerhead's ReBirth RB-338 (1997) put in a computer what a techno
 * producer's studio was: two Bass Lines, an 808 (and from version 2.0,
 * 1998, a 909), a mixer, a delay, a distortion, a compressor, and its
 * own idea, the PCF -- a pattern controlled filter, a low-pass whose
 * cutoff moves with the steps. It was the first software instrument
 * many people made records with, and the first sold as a replacement
 * for the machines it imitated.
 *
 * Ours is a hub over the voices already there -- Voice_tb303.mli twice,
 * Voice_tr808.mli as the 808 and as the 909 -- and what a hub adds is
 * the *one clock*: each machine keeps its own sequencer, sample-exact
 * (Sequencer.mli), so started at the same sample and at the same tempo
 * they stay together for ever, their steps falling on the same
 * samples. Then the mixer and the effects:
 *
 *     303 #1 --+--> distortion --+
 *     303 #2 --+                 +--> delay --> compressor --> out
 *     808 ----+--> PCF ----------+
 *     909 ----+    (its cutoff a pattern of 16)
 *
 * Ours, and said so: the effects' settings (the delay three sixteenths,
 * its feedback, the compressor's), the PCF's range (200 Hz to 20 kHz),
 * the levels.
 *
 * Worked example (Unit_rebirth): the four sequencers' steps the same
 * after every block for two bars; the machines all muted, silence; a
 * golden WAV per song. *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

(* the four machines, in the mixer's order *)
type machine = Bass1 | Bass2 | Drums808 | Drums909

val machines : machine list
val name : machine -> string
val index : machine -> int

type patch = {
  bass1 : Voice_tb303.patch;
  bass2 : Voice_tb303.patch;
  drums808 : Voice_tr808.patch;
  drums909 : Voice_tr808.patch;
  levels : float array; (* by [index], 0 to 1 *)
  mutes : bool array;
  distortion : float; (* on the 303s, 0 to 1 *)
  delay : float; (* its level, 0 to 1 *)
  compressor : bool;
  pcf : float array; (* 16 cutoffs, 0 to 1 *)
  pcf_on : bool;
  tempo : float; (* BPM, all four's *)
  volume : float;
}

val initial : patch

(* ours: acid, techno, house *)
val songs : (string * patch) list

(* [pcf_hz k]: the PCF's cutoff for a step's value, 200 x 100^k Hz *)
val pcf_hz : float -> float

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t

val create : patch -> t
val patch : t -> patch

(* the patch, its machines' and the tempo given to the four *)
val set_patch : t -> patch -> unit

(* the four started together, at the same sample; or stopped *)
val run : t -> bool -> unit
val running : t -> bool

(* each machine's step sounding, by [index] *)
val steps : t -> int array

(* the machines' own voices, for a panel to look into *)
val bass : t -> int -> Voice_tb303.t
val drums : t -> int -> Voice_tr808.t
val recent : t -> Signal.t
val instrument : t -> Instrument.t
