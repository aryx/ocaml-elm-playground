(* Reverb: a room's echoes, too many and too close to hear one by one
 * (see notes_synth.md section 8.6).
 *
 * A sound in a room reaches the ear directly, then off the walls, then
 * off the walls' echoes, thousands of paths, each a little later and
 * quieter: a tail, falling by 60 dB in the room's *reverberation time*
 * (a bathroom about 0.5 s, a hall 2, a cathedral 5 or more). Three
 * ways of making one, in the order they were found, all live, all
 * with that time as their knob:
 *
 * {1 Schroeder's (1962)}
 *
 * Synth.reverb's, streaming: four feedback combs side by side (delay
 * lines of 30 to 44 ms fed back into themselves), then two all-passes
 * in a row to thicken the echoes without colouring them:
 *
 *     x --+--> comb 29.7 ms --.
 *         +--> comb 37.1 ms --+--> ( + ) --> all-pass 5 ms --> all-pass 1.7 ms
 *         +--> comb 41.1 ms --+
 *         '--> comb 43.7 ms --'
 *
 * A comb of delay D loses 60 dB in T when its feedback is 10^(-3 D / T),
 * so every comb dies in the same time. Its echoes are regular (four
 * combs, a few hundred echoes a second): metallic on a drum.
 *
 * {1 Freeverb (Jezar Wakefield, 2000, public domain)}
 *
 * The same plan, bigger: eight combs (25 to 37 ms, their lengths chosen
 * to share no factor, so their echoes don't line up), four all-passes,
 * and two things Schroeder's lacks:
 *
 *  - a one-pole low-pass *inside* each comb's loop ([damping]): each
 *    trip round the comb darker, the highs dying first, as a room's
 *    air and soft walls absorb them;
 *  - two channels, the right's lines all 23 samples longer than the
 *    left's: the same room heard from two slightly different places,
 *    so the tail is wide, not in the middle of the head.
 *
 * Jezar's knob is the "room size", one feedback for all eight combs
 * (0.7 to 0.98), so its combs die at different times, the longest last
 * (his default 0.84: 1.2 s for his 1356-sample comb); here each comb's
 * feedback is set from the time, Schroeder's rule, to make [seconds]
 * the knob.
 *
 * {1 Dattorro's plate (1997)}
 *
 * The reverb of 1980s studio hardware (Lexicon, EMT's plate: a sheet of
 * steel with a pickup): the input diffused by four all-passes into a
 * smear, then into a *tank*, a figure eight of two halves, each feeding
 * the other:
 *
 *               .--------------------- x decay <-------------------.
 *               v                                                  |
 *     in --> ( + ) --> mod all-pass --> delay --> damp --> x decay --> all-pass --> delay --.
 *     in --> ( + ) --> mod all-pass --> delay --> damp --> x decay --> all-pass --> delay --+
 *               ^                                                                          |
 *               '------------------------------ x decay <--------------------------------'
 *
 * the output read from fourteen *taps* along the tank (seven a side),
 * mostly from the half opposite: never the same path twice, so the
 * echoes are dense and irregular from the start. The first all-pass of
 * each half has its length wobbling (16 samples at 1 Hz): the echoes'
 * pattern keeps changing, no resonance can build up, and the tail is
 * smooth where a still one rings. A sound goes round the whole loop
 * (0.725 s of delays) through [decay] four times, so decay = 10^(-3 x
 * 0.725 / (4 T)) for a time T.
 *
 * Worked example (Unit_reverb): a click through each, the time to fall
 * 60 dB (the energy in 50 ms windows, the slope fitted between -5 and
 * -35 dB, as acousticians measure a room: T30, doubled):
 *
 *     set to              1 s      2 s      (no damping)
 *     Schroeder           1.02     2.00
 *     Freeverb            1.00     2.00
 *     plate               1.14     1.94
 *
 * The combs obey the rule exactly; the plate's all-passes and its
 * modulation add a little (the rule counts the delays only). With the
 * damping at 1 and 2 s, the whole tail falls in 1.59 s (Freeverb) and
 * 1.71 s (plate), its highs (above 4 kHz) in 0.58 s and 1.24 s: the
 * low-pass in the loop, the room's absorption.
 *
 * References: Manfred R. Schroeder, "Natural Sounding Artificial
 * Reverberation", JAES 10(3), 1962; Jezar Wakefield, Freeverb, 2000
 * (its C++ source, public domain); Jon Dattorro, "Effect Design, Part
 * 1: Reverberator and Other Filters", JAES 45(9), 1997; Julius O.
 * Smith III, Physical Audio Signal Processing, "Artificial
 * Reverberation", https://ccrma.stanford.edu/~jos/pasp/. *)

type kind = Schroeder | Freeverb | Plate

val kinds : kind list
val name : kind -> string

type settings = {
  kind : kind;
  seconds : float; (* the time to fall 60 dB *)
  damping : float; (* 0 to 1: Freeverb's and the plate's highs dying sooner *)
  mix : float; (* the reverb's level, 0 to 1, the dry sound kept *)
}

(* the three's lines *)
type t

val create : unit -> t

(* [process t settings s]: [s] in place, both channels (the input their
 * mix, the reverb in stereo) *)
val process : t -> settings -> Signal.stereo -> unit

(* {1 As an effect} *)

(* kind (schroeder, freeverb, plate: the plate), time (0.3 to 8 s: 2),
 * damping (0 to 1: 0.3), mix (0 to 1: 0.25) *)
val knobs : Effect.knob list

(* [effect ()]: "reverb" *)
val effect : unit -> Effect.t
