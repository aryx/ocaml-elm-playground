(* Reason's rack: devices, the cables between their jacks, and the sound
 * they make together (plan_tiny_reason.md, TinyReason).
 *
 * Propellerhead's Reason (2000) came after ReBirth (Studio_rebirth.mli)
 * and undid its one fixed studio: a rack you fill with devices, a
 * mixer, synthesizers, a drum machine, sequencers, effects, and on its
 * back, cables from jack to jack. The rack is a *graph*, and a sound
 * is the graph evaluated:
 *
 *   matrix --Note, Gate--> juno --Audio--> mixer ch1 --+
 *   tr808 ---------------------Audio-----> mixer ch2 --+--> hardware
 *              mixer Aux Send --> delay --> Aux Return
 *
 * Each chunk (Rack_device.chunk, 64 samples), every device's stages
 * run in a *topological order* -- a stage after every stage that feeds
 * it -- so that what an input reads was written this chunk: Kahn's
 * algorithm (1962), a node taken when nothing left feeds it, the ties
 * by the rack's order, top to bottom, so that the order, and the
 * samples, are the same whatever order the cables were plugged in. A
 * cable that would close a loop has no such order, and is refused.
 *
 * The patch is data: the devices (their kinds, top to bottom) and the
 * cables. The running devices are made by the host (a device's front
 * and its sound share a voice: Rack_module.mli) and attached here by
 * their ids.
 *
 * Ours, and said so: an input takes one cable and an output one (fan-out
 * is Reason's Spider's, left out); a stereo cable per jack (Reason has
 * L and R); the CVs change at chunks. *)

type id = int
type port = { device : id; jack : int }
type cable = { out : port; into : port }

type patch = {
  devices : (id * string) list; (* their kinds, the rack top to bottom *)
  cables : cable list;
  tempo : float;
  volume : float;
  next : id; (* the id the next device gets *)
}

(* the Hardware Interface, always at the top: its one input (jack 0) is
 * what comes out of the computer *)
val hardware : id
val hardware_device : Rack_device.t

(* the Hardware Interface alone, 120 BPM *)
val empty : patch

(*****************************************************************************)
(* {1 The graph} *)
(*****************************************************************************)

(* the pure functions over a patch look its devices' jacks and stages
 * up by id *)
type lookup = id -> Rack_device.t

(* the stages in the order they run, (device, stage index); None if the
 * cables close a loop *)
val order : lookup -> patch -> (id * int) list option

(* [connect lookup patch cable]: the cable plugged, replacing any other
 * in its input or from its output; or why not: "In to In", "Out to
 * Out", "audio into CV", "CV into audio", "a loop through the delay" *)
val connect : lookup -> patch -> cable -> (patch, string) result

(* the cables at a jack pulled out *)
val disconnect : patch -> port -> patch

(* the cable into an input, or out of an output *)
val cable_at : patch -> port -> cable option

(* [add patch ~kind ~below]: a device of that kind put in the rack,
 * under [below] (at the bottom if None), and its id -- not yet cabled *)
val add : patch -> kind:string -> below:id option -> patch * id

(* [route lookup patch id ~selected]: the device [id], just added,
 * cabled as Reason does it by itself: an instrument to the first free
 * mixer channel (or the hardware); a mixer to the hardware; a
 * sequencer's note and gate into the selected instrument; an effect
 * with the mixer selected as its send and return, with an instrument
 * selected inserted after it *)
val route : lookup -> patch -> id -> selected:id option -> patch

(* a device taken out, its cables with it -- an effect inserted between
 * two others leaving them joined *)
val remove : lookup -> patch -> id -> patch

(*****************************************************************************)
(* {1 Playing it} *)
(*****************************************************************************)

type t

(* the hardware attached; the others by [attach] *)
val create : patch -> t

val attach : t -> id -> Rack_device.t -> unit
val lookup : t -> lookup
val patch : t -> patch

(* the patch's cables and order; devices no longer in it let go; the
 * tempo given to all *)
val set_patch : t -> patch -> unit

(* every device started together, in the same chunk, or stopped *)
val run : t -> bool -> unit
val running : t -> bool

(* the loudest sample of an output's last chunk *)
val peak : t -> port -> float

(* the last 2048 samples out *)
val recent : t -> Signal.t

(* the rack as the mixer pulls it; its keys play nothing (a host plays
 * a device's) *)
val instrument : t -> Instrument.t
