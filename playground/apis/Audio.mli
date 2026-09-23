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
   clicks (Envelope.mli), FM (Fm.mli), filters (Filter.mli), and a mixer
   adding it all up (Mixer.mli).
*)

open Playground

(* A sound: what [play] and [keep_playing] take. *)
type sound

(*****************************************************************************)
(* {1 Making sounds} *)
(*****************************************************************************)

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

(* [pluck frequency]: a plucked string, a guitar's or a harp's, bright
   then mellow as it rings (Pluck.mli: noise in a delay line,
   averaged as it goes round, Karplus and Strong's discovery):
     let strum = together [ pluck 196; pluck 247; pluck 294 ] |> lasting 2 *)
val pluck : number -> sound

(* [noise roughness]: random, for explosions, hits, wind, drums;
   [roughness] from 500 (a rumble) to 10,000 (a hiss) *)
val noise : number -> sound

(* [fm frequency ratio depth]: two sines, one wobbling the other (John
   Chowning's FM, the Yamaha DX7's and the Sega Genesis's sound:
   Fm.mli): [ratio] a whole number for an instrument (1 brass, 2
   a clarinet), in between for a bell or a gong (1.4); [depth] from 0 (a
   plain tone) to 10 (very bright). Fading, it gets darker as it dies
   away, like a struck bell:
     let bell = fm 440 1.4 5 |> lasting 2 |> fading *)
val fm : number -> number -> number -> sound

(* [note name]: the tone of a note, "A4" (440 Hz), "C4" (middle C),
   "F#5", "Bb3" (audio/Music.mli: equal temperament); silent if it isn't
   a note *)
val note : string -> sound

(*****************************************************************************)
(* {1 Shaping them, like move and scale for shapes} *)
(*****************************************************************************)

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

(* [low_pass cutoff s], [high_pass cutoff s]: [s] with its frequencies
   above [cutoff] (in hertz) taken away, or those below: muffled, or
   thin. Noise low-passed is a rumble, an engine, an explosion; high-
   passed, a hiss, a cymbal (Filter.mli):
     let engine = noise 3000 |> low_pass 300 *)
val low_pass : number -> sound -> sound
val high_pass : number -> sound -> sound

(* [wah from to_ s]: [s] through a low-pass that rings at its cutoff,
   the cutoff moving from [from] to [to_] over the sound: the "wah" of
   the analog synthesizers, sweeping through the harmonics of a rich
   sound (a sawtooth, a square):
     let wow = sawtooth 110 |> lasting 1 |> wah 200 4000 *)
val wah : number -> number -> sound -> sound

(* [vibrato rate depth s]: the pitch wobbling [depth] semitones up and
   down, [rate] times a second: 6 and 0.3 a singer, 8 and 2 a siren
   (audio/Pitch_effect.mli) *)
val vibrato : number -> number -> sound -> sound

(* [arpeggio semitones step s]: the pitch stepping through [semitones]
   above the note, [step] seconds each, around and around: [0; 4; 7] is
   a major chord played by a single voice, the chiptune trick of the
   NES and the C64, which had too few voices for real chords:
     let chord = square 262 |> arpeggio [ 0; 4; 7 ] 0.03 |> lasting 1 *)
val arpeggio : number list -> number -> sound -> sound

(* [echo delay feedback s]: [s] again [delay] seconds later, [feedback]
   as loud (0 to 0.95), and again, and again, dying away: a cave, a
   canyon (the sound lasts longer: until the echoes are silent) *)
val echo : number -> number -> sound -> sound

(* [reverb seconds s]: [s] in a room whose echoes die away in [seconds]
   (0.5 a bathroom, 2 a hall, 5 a cathedral): Schroeder's reverb,
   Synth.mli; the sound that much longer *)
val reverb : number -> sound -> sound

(* The live effects of a synthesizer's rack (notes_synth.md section 8),
   on a sound:

     let guitar = pluck 110 |> lasting 1 |> drive 18 |> chorus

   [drive gain s]: [s] pushed [gain] dB into a tanh, oversampled
   (Drive.mli), and brought [gain] dB back down: an overdriven
   amplifier at the sound's own level, a quiet sound passing as it was,
   a loud one flattened (its peaks at most 1 / 10^(gain / 20): 0.25 for
   12 dB), what's left the distortion *)
val drive : number -> sound -> sound

(* [chorus s]: [s] with a copy wobbling slightly out of tune, 15 ms
   behind, the two sides differently: several players, wide
   (Modulated_delay.mli); [flanger s]: the jet plane, notches sweeping;
   [phaser s]: a softer sweep (Phaser.mli) *)
val chorus : sound -> sound
val flanger : sound -> sound
val phaser : sound -> sound

(* [compressed threshold ratio s]: [s]'s loud parts turned down, above
   [threshold] dB, [ratio] dB in for 1 out (4: a gentle compressor, 20:
   nearly a limiter) (Dynamics.mli) *)
val compressed : number -> number -> sound -> sound

(* [naive s]: the square, triangle and sawtooth waves of [s] computed
   the simple way, a formula, with their aliases: high notes whistle
   out of tune (the default removes most of them: Oscillator.mli;
   examples/AudioAliasing.ml lets you hear both) *)
val naive : sound -> sound

(*****************************************************************************)
(* {1 Where it comes from} *)
(*****************************************************************************)

(* [pan p s]: [s] from the left (-1), the middle (0), the right (1), or
   in between; its loudness the same wherever it is (audio/Space.mli:
   the constant power law) *)
val pan : number -> sound -> sound

(* [from x y s]: [s] from that point of the screen, heard from its
   centre: panned by x (the screen's edges, x = -500 and 500, the
   speakers), and quieter off the screen, further than 700 (half as loud
   at 1400: the inverse distance law):
     Audio.play (explosion |> Audio.from asteroid.x asteroid.y) *)
val from : number -> number -> sound -> sound

(* [pitched k s]: every pitch of [s] times [k] (2: an octave up), its
   length kept; with [faster k], a Doppler shift: [k] from
   audio/Space.mli's [doppler], the pitch of a car rising as it comes
   and falling as it goes *)
val pitched : number -> sound -> sound

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

(* [of_tune tune]: a tune already parsed (Abc.parse), or changed after
   parsing -- a voice muted, a single note kept -- played the same way *)
val of_tune : Abc.tune -> sound

(* [wav bytes]: a recording, a WAV file's sound (its bytes, as read from
   a .wav file: 16-bit, mono or stereo, any sample rate; audio/Wav.mli);
   [pitched] then plays it higher or lower, and shorter or longer with
   it, as a tape would (Resample.mli) *)
val wav : string -> sound

(* [recorded s]: [s] frozen into a recording, its samples computed once:
   from then on, like a WAV file's (pitched: faster and higher, as a
   sampler plays one recording at every key; examples/AudioSampler.ml) *)
val recorded : sound -> sound

(* [midi bytes]: a Standard MIDI File's music (its bytes, as read from
   a .mid file: audio/Midi.mli), General MIDI's instruments played by
   our 8-bit ones, drums on noise (audio/Music.mli) *)
val midi : string -> sound

(*****************************************************************************)
(* {1 Ready-made sounds} *)
(*****************************************************************************)
(* In the spirit of sfxr (Tomas Pettersson, 2007), the game jam tool
   whose few parameters make most 8-bit game sounds: each here is a
   handful of numbers (audio/Sfx.mli; examples/AudioSfx.ml plays them
   and shows the numbers). *)

val blip : sound (* a short beep: a menu, a ball on a paddle *)
val coin : sound (* two quick rising notes: a pickup *)
val jump : sound (* a quick rising square *)
val laser : sound (* a falling sawtooth *)
val hit : sound (* a short noise burst *)
val explosion : sound (* a long, falling noise *)
val step : sound (* a soft, low tick: footsteps *)
val powerup : sound (* a rising, warbling square *)

(* [varied name seed]: the ready-made sound [name] ("blip", "coin", ...)
   with its numbers nudged at random (sfxr's "mutate"), the same for the
   same [seed]: ten shots in a row that don't all sound alike,
     Audio.play (Audio.varied "laser" shots_fired) *)
val varied : string -> int -> sound

(* [random_sound category seed]: sfxr's "random" buttons: a new sound of
   that kind ("laser", "explosion", "coin", "jump", "powerup", "hit",
   "blip"), its numbers drawn afresh, the same for the same [seed]: a
   game's sounds found by pressing a button until one sounds right *)
val random_sound : string -> int -> sound

(* [sfx numbers]: your own, from sfxr's numbers (audio/Sfx.mli):
     let zap = sfx { Sfx.laser with frequency = 2000.; echo = 0.1 } *)
val sfx : Sfx.t -> sound

(*****************************************************************************)
(* {1 Playing them} *)
(*****************************************************************************)

(* [play s]: from now until its end (see the top) *)
val play : sound -> unit

(* [keep_playing name s]: [s] playing while this is called every frame
   (its pan too, gliding when it changes: a car going by),
   [name] saying it's the same sound from frame to frame (its length
   and fading ignored: it lasts as long as it's kept); a filter's cutoff
   may change from frame to frame, a ship's engine brighter as it
   speeds up (a wah's sweep ignored: the cutoff is the frame's):
     Audio.keep_playing "thrust" (Audio.noise 2000 |> Audio.low_pass (200 + speed)) *)
val keep_playing : string -> sound -> unit

(* [loop name s]: [s] played over and over, background music, until
   [stop name]; calling it again (at every frame, say) while it plays
   does nothing, so a game can just say, in update:
     Audio.loop "music" tune *)
val loop : string -> sound -> unit
val stop : string -> unit

(* [faster k s]: [s] [k] times as fast, the same notes (1.5: half as
   fast again): a tune's tempo *)
val faster : number -> sound -> sound

(* [change_loop name s]: the loop [name] now plays [s], going on from
   the same point of the tune (the same fraction of the way through),
   not from its start: the music speeding up with the danger, as in
   Space Invaders and Tetris,
     Audio.change_loop "music" (tune |> Audio.faster (1 + 0.1 * level)) *)
val change_loop : string -> sound -> unit

(* [fetch source k]: a file's or a URL's bytes, got as [loop_from] gets
   them, given to [k] -- now natively, later in a browser, None if they
   can't be had: for a program that edits a song rather than only
   playing it (apps/music/TinySoundtracker.ml) *)
val fetch : string -> (string option -> unit) -> unit

(* [play_module name bytes]: a MOD file's song (its bytes, as read from
   a .mod: audio/formats/mod/Mod.mli), its own instruments playing its
   patterns on the Amiga's four channels (Mod_player.mli), looping until
   [stop name]; nothing if it isn't a module (the reason printed on the
   error output) *)
val play_module : string -> string -> unit

(* [loop_from name source]: a tune from a file or a URL, looping once
   it's there: a MIDI file if [source] ends in .mid, an ABC one in .abc,
   a recording in .wav, a module in .mod, else solfège; a local path
   natively, a URL anywhere (natively
   downloaded, blocking the first time; in a browser fetched in the
   background, from the page's own server for a plain name). Like
   [loop], calling it again while it plays (or downloads) does nothing:
     Audio.loop_from "music" "https://example.com/tune.mid" *)
val loop_from : string -> string -> unit

(*****************************************************************************)
(* {1 Instruments: played live} *)
(*****************************************************************************)
(* A sound above is made whole, then played. An instrument is played
   the way a piano is: a key goes down, and the note sounds until the key
   comes up, however long that is; and its knobs can be turned while it
   plays. In update:

     let keys = Audio.instrument "keys" Instrument.sine in
     if pressed "a" then Audio.note_on keys "C4";
     if released "a" then Audio.note_off keys "C4"

   An instrument is made the first time it's asked for, and kept by
   name: asking again (at every frame) gives the same one, until [stop
   name]. What instruments there are is audio/'s business
   (Instrument.mli: [sine], the smallest one; a synthesizer is a bigger
   one, apps/music/TinyMinimoog.ml's). *)

type instrument

(* [instrument name make]: the instrument [name], made by [make ()] if
   it isn't playing yet *)
val instrument : string -> (unit -> Instrument.t) -> instrument

(* [note_on i name]: the key of the note [name] ("C4", "F#5": see
   [note]) pressed, at full velocity; [note_off i name] let go.
   Nothing if [name] isn't a note. *)
val note_on : instrument -> string -> unit
val note_off : instrument -> string -> unit

(* [set i knob value]: a knob turned ("volume", 0 to 1, for
   Instrument.sine), smoothly: over the next few milliseconds, not at
   once, so nothing clicks *)
val set : instrument -> string -> number -> unit

(*****************************************************************************)
(* {1 The music's own clock} *)
(*****************************************************************************)

(* [position name]: how far into the loop [name] the sound card has
 * been fed, in seconds, counting every time round (so 35.2 for a 16 s
 * song on its third pass); None if it isn't playing.

   A game has two clocks, and for most games the difference never
   matters. For a rhythm game it is the whole difficulty:

     the game's clock: computer.time, advanced once a frame
       |------|------|------|---- - - --|------|------|  60 a second,
       frame  frame  frame     a frame  frame  frame     unless a frame
                                late                     is late
     the music's clock: this, advanced by the samples that went out
       ||||||||||||||||||||||||||||||||||||||||||||||||  44,100 a
                                                         second, never
                                                         late (or it
                                                         clicks)

   The two drift apart -- a slow frame, a window being dragged, a
   machine busy elsewhere -- and the music does not wait for the
   frames. So a step has to be judged against *this* clock, the one
   the player is dancing to; a game that judges it by computer.time is
   judging against a clock the player cannot hear.

   And even this is early: it counts what has been *given* to the
   sound card, which plays it a little later (natively the queue is
   kept ~50 ms ahead, in a browser ~100 ms, more over Bluetooth). That
   last gap depends on the machine, not the program, which is why
   every rhythm game since has a calibration screen: a number the
   player sets, subtracted from this one. *)
val position : string -> number option

(**/**)

(* claude: for the platforms (Playground_platform), not for games: the
 * next [n] samples of everything playing, at 44,100 a second, which the
 * native backend queues for SDL and a golden run writes to a WAV
 * (audio/Mixer.mli) *)
val pull : int -> Signal.stereo

(* claude: for the platforms too: how [loop_from] gets a file's bytes,
 * [fetch source k] calling [k] with them (None if it can't), now or
 * later; installed by Playground_platform.run_app (natively a file read
 * or a curl download, in a browser an XMLHttpRequest) *)
val set_fetcher : (string -> (string option -> unit) -> unit) -> unit
