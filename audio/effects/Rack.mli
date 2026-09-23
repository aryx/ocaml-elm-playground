(* The rack: effects one after the other, each switched on or not, in
 * an order that can change (see notes_synth.md section 8).
 *
 *     voice --> drive --> EQ --> delay --> reverb --> out     [standard]
 *               (phase 7: chorus after the EQ, the compressor last)
 *
 * The order matters, because the effects don't commute, and the
 * nonlinear one least of all: a drive after a reverb distorts the
 * whole tail, every echo of every note clipped *together* (their sum
 * through the curve: intermodulation, new tones at the differences of
 * the notes), mud; before it, each note is distorted alone, then the
 * room is added, clean. The EQ after the drive shapes what the drive
 * made (a treble cut tames its fizz); the delay before the reverb so
 * that its echoes are in the room too, as a guitarist's pedals go into
 * the amplifier in the hall. [reorder] puts the reverb first, to hear
 * the mud.
 *
 * The rack holds Effect.t's, not knowing what they are; their knobs
 * are its knobs, named "effect.knob" ("delay.time"), with an "on"
 * switch each ("delay.on", off at first: the sound goes through a
 * stage that's off untouched, the stage's state kept). *)

type t

(* [create effects]: a rack of [effects], in that order, all off *)
val create : Effect.t list -> t

(* drive, eq, delay, reverb: Drive, Eq, Delay and Reverb's effects *)
val standard : unit -> t

(* [standard]'s knobs, without making one (a patch's controls are
 * known before any sound): "drive.on", "drive.shape", ..., "reverb.mix" *)
val standard_knobs : Effect.knob list

(* [knobs t]: the effects' knobs under their names, each "on" first *)
val knobs : t -> Effect.knob list

(* [set t name x]: a knob turned, "delay.time" or "delay.on"; names it
 * doesn't have are ignored *)
val set : t -> string -> float -> unit

(* the effects' names, in their order *)
val order : t -> string list

(* [reorder t names]: the effects in that order (names missing keep
 * their place after those given; unknown ones ignored) *)
val reorder : t -> string list -> unit

(* [process t s]: [s] through the effects that are on, in place *)
val process : t -> Signal.stereo -> unit
