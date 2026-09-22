(* Effects: a sound changed over time, its pitch wobbling or jumping, or
 * echoed (see notes_audio.md section 8).
 *
 * The pitch effects multiply a voice's frequency by a factor that
 * changes with time t, the seconds since the voice started:
 *
 *   - a vibrato, the pitch wobbling up and down, [depth] semitones either
 *     way, [rate] times a second: 2^(depth sin (2 pi rate t) / 12). A
 *     singer's or a violinist's (5 to 7 a second, a quarter of a
 *     semitone); faster and deeper, a siren, a UFO.
 *   - a jump, [semitones] up (or down) once, at [at] seconds: a coin's
 *     two notes in one sound (sfxr's "change").
 *   - an arpeggio, the notes of a chord one after the other, [step]
 *     seconds each, around and around:
 *
 *         semitones  0  4  7  0  4  7  0 ...     C E G C E G: a C major
 *         pitch      ___                          chord, played by a
 *                       ___                       single voice
 *                          ___
 *
 *     the chiptune trick: the NES had two square channels, the C64
 *     three voices, too few to play chords; cycled every frame (1/60 s),
 *     the notes blur into one warbling chord. (Arpeggio machines and
 *     trackers' "0xy" effect: the note, x semitones up, y up, every
 *     tick.)
 *
 * The echo: the sound plus itself [delay] seconds later, [feedback] as
 * loud, and that echo echoed again, and so on, each [feedback] times the
 * last:
 *
 *     y[n] = x[n] + feedback y[n - delay rate]
 *
 * a feedback comb filter, whose memory is a delay line, the last
 * [delay] seconds of output (a circular buffer: 13,230 samples for 0.3
 * s). The echoes die away geometrically: the output lasts [tail] longer
 * than the input, the time for them to fall below -60 dB (a thousandth:
 * reverberation time's convention, Wallace Sabine's RT60, 1900).
 *
 * Example: feedback 0.5 and a delay of 0.25 s, the echoes at 0.5, 0.25,
 * 0.125...; the tenth (0.001) at 2.5 s: the tail.
 *
 * A reverb is a room's thousands of echoes, off every wall, too many
 * and too close together to hear one by one: a wash that dies away.
 * Manfred Schroeder (Bell Labs, 1962) made one out of the echo above:
 * four feedback combs in parallel, their delays around 30 to 45 ms and
 * mutually prime (29.7, 37.1, 41.1, 43.7 ms: their echoes never line up,
 * so they don't ring at one pitch), then two all-pass filters in series
 * (5.0 and 1.7 ms), which multiply the echoes without colouring the
 * sound -- an all-pass lets every frequency through at the same level,
 * only later:
 *
 *     y[n] = -g x[n] + x[n - D] + g y[n - D]
 *
 *     in --+--> comb 29.7 ms --+
 *          +--> comb 37.1 ms --+--> / 4 --> all-pass 5 ms --> all-pass 1.7 ms --> wet
 *          +--> comb 41.1 ms --+
 *          +--> comb 43.7 ms --+
 *
 * Each comb's feedback is set from the reverberation time asked for, T
 * (60 dB down after T seconds): a comb of delay D loses 60 dB in T when
 * its feedback is 10^(-3 D / T). A bathroom is about 0.5 s, a hall 2, a
 * cathedral 5 or more. It sounds metallic next to a modern reverb (the
 * combs' echoes are regular; Freeverb, 2000, uses eight, and a
 * convolution with a real room's recorded echo is exact): an exercise.
 *
 * References: Tomas Pettersson, sfxr, 2007 (vibrato and "change" among
 * its parameters); Julius O. Smith III, Physical Audio Signal
 * Processing, 2010, "Feedback Comb Filters" and "Schroeder
 * Reverberators", https://ccrma.stanford.edu/~jos/pasp/; Manfred R.
 * Schroeder, "Natural Sounding Artificial Reverberation", Journal of
 * the Audio Engineering Society 10(3), 1962. *)

type pitch =
  | Vibrato of { rate : float; depth : float } (* Hz, semitones *)
  | Jump of { semitones : float; at : float } (* at, in seconds *)
  | Arpeggio of { semitones : float list; step : float } (* each [step] s *)

(* [factor p t]: the frequency multiplier at [t] seconds in: 1 for no
 * change, 2 an octave up *)
val factor : pitch -> float -> float

(* [tail ~delay ~feedback]: how much longer the echo makes a sound, the
 * delay times the echoes it takes to fall below 0.001 (2.5 s for 0.25
 * and 0.5); [feedback] in [0, 1) *)
val tail : delay:float -> feedback:float -> float

(* [echo ~delay ~feedback s]: [s] echoed, [tail] longer *)
val echo : delay:float -> feedback:float -> Signal.t -> Signal.t

(* [reverb ~seconds ?mix s]: [s] plus [mix] (0.3 by default) of
 * Schroeder's reverb, [seconds] its reverberation time, the sound that
 * much longer *)
val reverb : seconds:float -> ?mix:float -> Signal.t -> Signal.t
