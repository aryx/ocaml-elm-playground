(* The mixer: every sound playing, summed into the samples the sound card
 * takes (see notes_audio.md sections 5 and 10). The stateful part of
 * audio/: the sounds come and go as the game plays them.
 *
 *    play blip ---.                        the one-shots: rendered once,
 *    play jump ---+--> [ + ] --> tanh -->  read through as the samples
 *    keep "thrust" --'   ^       samples   go out; the continuous ones:
 *                        |                 advanced frame by frame
 *               pull n: the next n
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

type t

val create : unit -> t

(* one-shots at most at once *)
val max_playing : int

(* [play m s]: [s] from the next sample pulled *)
val play : t -> Signal.t -> unit

(* [loop m name s]: [s] played over and over, from the next sample,
 * unless a loop [name] is already playing (so calling it every frame
 * is harmless): background music *)
val loop : t -> string -> Signal.t -> unit

(* [stop m name]: the loop [name] stopped (faded out over its next
 * pull), if playing *)
val stop : t -> string -> unit

(* [keep m name v]: the continuous voice [name] playing [v] until the
 * next pull at least *)
val keep : t -> string -> Synth.voice -> unit

(* [pull m n]: the next [n] samples *)
val pull : t -> int -> Signal.t

(* one-shots playing, continuous voices kept (for tests, debug) *)
val playing : t -> int * int

(* loops playing *)
val looping : t -> string list
