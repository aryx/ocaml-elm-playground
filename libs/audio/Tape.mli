(* The tape: a multitrack recorder, the OP-1's studio (see
 * notes_synth.md; plan_synth_teaching.md, TinyOp1, O1).
 *
 * Teenage Engineering's OP-1 (2011) has no song editor, no timeline:
 * it has a *tape*, four tracks, six minutes, as the Beatles had at Abbey
 * Road (four tracks, 1963) -- an instrument played onto a track, then
 * another onto the next while listening to the first, and so on. And a
 * tape is not a list of notes but *sound*, samples along a reel, with a
 * head that reads and writes where it stands:
 *
 *     track 1  ~~~~~~~~~~~~~~~~~~|~~~~~~~~
 *     track 2  ~~~~~~~   ~~~~~~~~|~~~
 *     track 3        ~~~~~~~~~~~~|~~~~~~~~~~~     the head: what's under it
 *     track 4                    |                heard, the input written
 *                                ^ head              (added: overdub)
 *
 * The tape moves under the head at a *speed*, 1 normal. Faster, the
 * recording is read faster: higher and shorter, together, as on a real
 * reel (Resample.mli: pitch and time go together); slower, lower and
 * longer; backwards, the speed negative. So the speed is also an
 * instrument: recorded at half speed and played at normal, a phrase
 * comes back an octave up, twice as fast -- the chipmunk records of
 * 1958, made this way.
 *
 * Reading falls between samples, as a pitch shift's does: the tracks
 * read with Resample's interpolation (linear). Writing too: at a speed
 * other than 1, an input sample lands between two tape samples, and is
 * spread over both, in proportion (the input sample's share of each):
 * crude, no filter, so a sound recorded slowly and played fast brings
 * its aliases (a real tape's head has its gap, a low-pass, for free).
 *
 * The rest of the OP-1's tape: *loop* points, the head jumping back from
 * the out point to the in point; and *lift* and *drop*, its cut and
 * paste: a piece of a track lifted into memory (silence left behind),
 * dropped at the head as many times as wanted (added, as recording is).
 * Recording always overdubs: new sound is added to the old, as the
 * OP-1's tape does ("always overdubs if there's recorded material").
 *
 * Worked example (Unit_tape): a phrase, a 344.5 Hz sine for a second
 * (bin 32 of a 4096-point spectrum), recorded on track 1 at speed 1:
 * played back at 1, the phrase exactly; at 2, bin 64, an octave up, and
 * silent after sample 22,049, half the time; at -1, the recording
 * backwards, sample for sample; recorded at 0.5 and played at 1: bin 64
 * again, silent after 22,050 (the last input sample written at
 * 22,049.5, half of it on the next); a second pass on the same track:
 * the two added; a piece lifted and dropped further on: moved.
 *
 * References: Teenage Engineering's OP-1 guide, "tape mode"
 * (https://teenage.engineering/guides/op-1/original/tape-mode: 6
 * minutes, 4 tracks, the speed turned even while recording, lift and
 * drop, loop in and out). *)

type t

(* [create ?seconds ?tracks ()]: six minutes, four tracks by default,
 * blank *)
val create : ?seconds:float -> ?tracks:int -> unit -> t

val tracks : t -> int
val length : t -> int (* samples *)

(* a track's samples (for a panel to draw, a test to read) *)
val track : t -> int -> Signal.t

(* the head, in samples from the start (fractional), and moving it *)
val head : t -> float
val set_head : t -> float -> unit

(* the speed: 1 normal, 2 twice as fast (an octave up), -1 backwards *)
val speed : t -> float
val set_speed : t -> float -> unit

(* the transport: [play] reads, [record t k] reads and writes onto track
 * [k] (0 to tracks - 1), [stop] neither (the head stays) *)
val play : t -> unit
val record : t -> int -> unit
val stop : t -> unit
val moving : t -> bool
val recording : t -> int option

(* a track's level in the mix, 0 to 1, 1 at first *)
val set_level : t -> int -> float -> unit

(* [set_loop t (Some (a, b))]: the head going back to [a] when it
 * passes [b] (or to [b] from [a], backwards); None: no loop *)
val set_loop : t -> (int * int) option -> unit

(* [process t ~input out]: a block: the tape moving [speed] samples a
 * sample (stopped at its ends), the tracks read at the head and mixed
 * into [out] (written over), [input] added onto the track recording *)
val process : t -> input:Signal.t -> Signal.t -> unit

(* [lift t k ~from ~until]: track [k]'s samples in [from, until) into
 * memory, silence left behind; [drop t k]: memory added onto track
 * [k] at the head *)
val lift : t -> int -> from:int -> until:int -> unit
val drop : t -> int -> unit
