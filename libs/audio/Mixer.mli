(* The mixer: every sound playing, summed into the samples the sound card
 * takes (see notes_audio.md sections 5 and 10). The stateful part of
 * audio/: the sounds come and go as the game plays them.
 *
 *    play blip ---------.                        the one-shots: rendered
 *    play jump ---------+--> [ + ] --> tanh -->  once, read through as the
 *    keep "thrust" -----+      ^       samples   samples go out; the
 *    instrument "moog" -'      |                 continuous ones: advanced
 *                              |                 frame by frame; the
 *                     pull n: the next n         instruments: asked for
 *                                                the next block
 *
 * Two clocks: the game's, 60 frames a second, and the sound card's,
 * 44,100 samples a second, pulling on its own (notes_audio.md section
 * 10). The platform calls [pull] with how many samples the card needs
 * (native: SDL's queue topped up to ~50 ms ahead; a golden run: exactly
 * 735 a frame, 44,100 / 60), so the mixer never waits nor rushes. A
 * one-shot starts at the next sample pulled; a continuous sound is
 * [keep]ed each frame, and when a [pull] finds it not kept since the
 * last one, it fades out over that pull and is gone.
 *
 * The sum goes through tanh (Mix.soft_clip): many sounds at once get
 * squeezed rather than cut flat. At most [max_playing] one-shots: past
 * it, the oldest is dropped (a backend not pulling, the web one before
 * phase 4, can't pile them up forever).
 *
 * References: Game Programming Patterns, "Event Queue" (Robert
 * Nystrom, 2014: playing sounds from game code, and why a queue); SDL's
 * SDL_QueueAudio, https://wiki.libsdl.org/SDL2/SDL_QueueAudio *)

(*****************************************************************************)
(* {1 The mixer} *)
(*****************************************************************************)

type t

val create : unit -> t

(* one-shots at most at once *)
val max_playing : int

(* false: every pull mixed down to one channel, the same in both (the
 * software backend's "m" key, to hear what panning does); true by
 * default *)
val stereo : bool ref

(*****************************************************************************)
(* {1 Playing sounds} *)
(*****************************************************************************)

(* [play m s]: [s] from the next sample pulled *)
val play : t -> Signal.stereo -> unit

(* [loop m name s]: [s] played over and over, from the next sample,
 * unless a loop [name] is already playing (so calling it every frame
 * is harmless): background music *)
val loop : t -> string -> Signal.stereo -> unit

(* [change m name s]: the loop [name] now [s], from the same point of
 * it (the same fraction of the way through: a tune made faster goes
 * on from the same note), its clock ([played]) going on; a new loop if
 * none is playing *)
val change : t -> string -> Signal.stereo -> unit

(* [stop m name]: the loop or the instrument [name] stopped (faded out
 * over its next pull), if playing *)
val stop : t -> string -> unit

(* [keep m name v]: the continuous voice [name] playing [v] until the
 * next pull at least, through [filter] if given (its cutoff fixed at
 * [cutoff]; changed from frame to frame, the filter's memory kept),
 * panned by [pan] (0, the middle, by default; changed from frame to
 * frame, it glides over the pull) *)
val keep : ?filter:Synth.filter -> ?pan:float -> t -> string -> Synth.voice -> unit

(* [instrument m name i]: the instrument [i] (Instrument.mli) playing
 * from the next pull, a block of it each pull, until [stop m name];
 * nothing if an instrument [name] is already playing (so asking for it
 * every frame is harmless, as for [loop]). Its notes and knobs are the
 * caller's, through [i]'s functions: the mixer only pulls. *)
val instrument : t -> string -> Instrument.t -> unit

(*****************************************************************************)
(* {1 Pulling samples} *)
(*****************************************************************************)

(* [pull m n]: the next [n] samples, in both channels *)
val pull : t -> int -> Signal.stereo

(*****************************************************************************)
(* {1 What is playing} *)
(*****************************************************************************)

(* one-shots playing, continuous voices kept (for tests, debug) *)
val playing : t -> int * int

(* loops playing *)
val looping : t -> string list

(* instruments playing (not those being stopped) *)
val instruments : t -> string list

(* [played m name]: how many samples of the loop [name] have gone out,
 * counting every time round -- the loop's own clock, which unlike its
 * read position does not go back to 0 at the end. None if no such loop
 * is playing. It is what a rhythm game has to time its steps by
 * (Audio.mli's [position], TinyDDR.ml): the frame
 * clock is the game's, and this one is the music's. *)
val played : t -> string -> int option
