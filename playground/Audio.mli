(* Audio: sounds for the playground's games.

   A sound is a value, like a shape: made from a few numbers, shaped by
   a few verbs, and played when something happens:

     (* in update, when the ball hits the paddle *)
     Audio.play blip

     (* your own: a laser, falling from high to low, dying away *)
     let zap = sawtooth 1200 |> sliding 200 |> lasting 0.2 |> fading

   Three things to know:

   - [play] is called in [update], at the moment something happens (a
     bounce, a jump, a coin): fire and forget, the sound plays by itself
     until its end. It's the one impure thing in the playground:
     elsewhere [update] only computes the new model. (If [update] ran
     twice for the same frame -- a time-travel debugger replaying it --
     the sound would play twice: the price of the simplicity. Elm's
     elm-audio avoids it by describing what's playing from the model,
     like [view] does for pictures.)
   - A sound that lasts as long as something goes on -- a ship's thrust,
     a theremin's note -- is [keep_playing name sound], called at every
     frame while it should sound: it stops by itself at the first frame
     it isn't called, and changing its pitch or volume from frame to
     frame changes it smoothly.
   - Sounds combine like Paul Hudak's music values in Euterpea (The
     Haskell School of Music): [after] plays sounds one after the other,
     [together] at the same time (his (:+:) and (:=:)):
       let coin = after [ square 1047 |> lasting 0.07; square 1568 |> fading ]

   Underneath is a small synthesizer written to be read, audio/ (see
   docs/claude_notes/notes_audio.md): oscillators (Oscillator.mli),
   noise from the NES's shift register (Noise.mli), envelopes so nothing
   clicks (Envelope.mli), and a mixer adding it all up (Mixer.mli).
*)

open Playground

(* A sound: what [play] and [keep_playing] take. *)
type sound

(* {1 Making sounds} *)

(* [tone frequency]: a pure tone, a sine wave, in hertz (440 is the A
   orchestras tune to); like every sound here, 0.3 s long at half volume
   until changed by the verbs below *)
val tone : number -> sound

(* the retro ones, the waveforms of the NES and the arcades: a square
   (hollow, a clarinet: the NES's melodies), a triangle (soft: the NES's
   bass), a sawtooth (buzzy: brass, lasers) *)
val square : number -> sound
val triangle : number -> sound
val sawtooth : number -> sound

(* [noise roughness]: random, for explosions, hits, wind, drums;
   [roughness] from 500 (a rumble) to 10,000 (a hiss) *)
val noise : number -> sound

(* [note name]: the tone of a note, "A4" (440 Hz), "C4" (middle C),
   "F#5", "Bb3" (audio/Music.mli: equal temperament); silent if it isn't
   a note *)
val note : string -> sound

(* {1 Shaping them, like move and scale for shapes} *)

(* [lasting seconds s] *)
val lasting : number -> sound -> sound

(* [fading s]: dies away over its length, like a plucked string or a
   drum, instead of stopping at once *)
val fading : sound -> sound

(* [louder k s]: [k] times as loud (0.5: half, 2: twice) *)
val louder : number -> sound -> sound

(* [sliding frequency s]: its pitch moving to [frequency] over its
   length: up for a jump, down for a laser or a falling bomb *)
val sliding : number -> sound -> sound

(* [together sounds]: at the same time, a chord; lasting as long as the
   longest *)
val together : sound list -> sound

(* [after sounds]: one after the other, a tune *)
val after : sound list -> sound

(* [abc text]: a tune written in ABC notation (audio/Abc.mli: letters
   for notes, numbers for lengths, a key, voices), played by an 8-bit
   band (the melody on a square wave, the bass on a triangle, like the
   NES):
     let tune = abc "X:1\nL:1/8\nQ:1/4=120\nK:C\nC2 E2 G2 c2 | G8 |"
   silent if the text isn't a tune (the reason printed on the error
   output) *)
val abc : string -> sound

(* [doremi text]: the same in solfège (audio/Doremi.mli): do ré mi fa
   sol la si, a length in beats after a colon, a rest -:
     let lune = doremi "do do do re mi:2 re:2 | do mi re re do:4"
   (Au clair de la lune) *)
val doremi : string -> sound

(* [midi bytes]: a Standard MIDI File's music (its bytes, as read from
   a .mid file: audio/Midi.mli), General MIDI's instruments played by
   our 8-bit ones, drums on noise (audio/Music.mli) *)
val midi : string -> sound

(* {1 Ready-made sounds}

   In the spirit of sfxr (Tomas Pettersson, 2007), the game jam tool
   whose few parameters make most 8-bit game sounds: each here is a few
   of the verbs above (see Audio.ml). *)

val blip : sound (* a short beep: a menu, a ball on a paddle *)
val coin : sound (* two quick rising notes: a pickup *)
val jump : sound (* a quick rising square *)
val laser : sound (* a falling sawtooth *)
val hit : sound (* a short noise burst *)
val explosion : sound (* a long, falling noise *)
val step : sound (* a soft, low tick: footsteps *)

(* {1 Playing them} *)

(* [play s]: from now until its end (see the top) *)
val play : sound -> unit

(* [keep_playing name s]: [s] playing while this is called every frame,
   [name] saying it's the same sound from frame to frame (its length
   and fading ignored: it lasts as long as it's kept) *)
val keep_playing : string -> sound -> unit

(* [loop name s]: [s] played over and over, background music, until
   [stop name]; calling it again (at every frame, say) while it plays
   does nothing, so a game can just say, in update:
     Audio.loop "music" tune *)
val loop : string -> sound -> unit
val stop : string -> unit

(* [loop_from name source]: a tune from a file or a URL, looping once
   it's there: a MIDI file if [source] ends in .mid, an ABC one in .abc,
   else solfège; a local path natively, a URL anywhere (natively
   downloaded, blocking the first time; in a browser fetched in the
   background, from the page's own server for a plain name). Like
   [loop], calling it again while it plays (or downloads) does nothing:
     Audio.loop_from "music" "https://example.com/tune.mid" *)
val loop_from : string -> string -> unit

(**/**)

(* claude: for the platforms (Playground_platform), not for games: the
 * next [n] samples of everything playing, at 44,100 a second, which the
 * native backend queues for SDL and a golden run writes to a WAV
 * (audio/Mixer.mli) *)
val pull : int -> float array

(* claude: for the platforms too: how [loop_from] gets a file's bytes,
 * [fetch source k] calling [k] with them (None if it can't), now or
 * later; installed by Playground_platform.run_app (natively a file read
 * or a curl download, in a browser an XMLHttpRequest) *)
val set_fetcher : (string -> (string option -> unit) -> unit) -> unit
