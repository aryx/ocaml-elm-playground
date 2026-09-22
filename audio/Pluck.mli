(* A plucked string: the Karplus-Strong algorithm (see notes_audio.md
 * section 8).
 *
 * Every other sound here is an oscillator, a formula of the phase, or
 * noise. A plucked string is neither, and yet it takes a dozen lines:
 * Kevin Karplus and Alex Strong (Stanford, 1983) found that a delay line
 * one period long, filled with noise and read around and around, each
 * sample written back as the average of two neighbours, sounds like a
 * guitar string:
 *
 *        +-------------------------------+
 *        |  delay line, p samples: noise | ---+---> out
 *        +-------------------------------+    |
 *                    ^                        |
 *                    +-- (x[n] + x[n+1]) / 2 -+   the average: a low-pass
 *
 * Why it works: read around and around, the line repeats every p
 * samples, so whatever is in it becomes a periodic wave, a pitch,
 * rate / p; the noise gives that wave every harmonic at once, like a
 * string plucked (a sharp pluck is a jump, rich in harmonics). The
 * averaging is a gentle low-pass applied once per period: the high
 * harmonics die first and the low ones last, which is exactly what a
 * real string does (its energy leaks faster at high frequencies) --
 * the bright twang turning to a mellow hum. Nothing models a string;
 * the ear only hears the spectrum's shape over time, and it's right.
 *
 * The pitch: the average of two neighbours is half a sample late, so the
 * period is p + 1/2 samples, and p = round (rate / frequency - 1/2).
 * Being whole, p can be off by up to half a sample: at A4, p = 100,
 * 438.8 Hz, 5 cents flat (up to 9 at that length); more for high notes
 * (2 kHz: p = 22, 1960 Hz, 35 cents, a third of a semitone); the fix,
 * a fractional delay (an all-pass filter, David Jaffe and Julius Smith,
 * 1983), is an exercise. [decay] shortens the ring a little more at
 * each pass, for the high notes, whose line is short and goes round
 * often. The noise comes from 5000 steps into the LFSR's sequence (its
 * first bits, from 1, are mostly 0s: a first try averaged -0.49, a
 * string three quarters -1s), and has its average taken out: 200 random
 * -1s and 1s don't add up to 0 exactly, and the averaging, a low-pass,
 * would keep that constant (0 Hz passes), the string ringing around an
 * offset.
 *
 * Example: A3 (220 Hz): p = round (200.45 - 0.5) = 200, the period 200.5
 * samples, 219.95 Hz. Its spectrum's centroid falls as it rings, from
 * 4312 Hz at the pluck to 644 Hz 1.5 s later (Unit_pluck): the twang,
 * then the hum.
 *
 * References: Kevin Karplus, Alex Strong, "Digital Synthesis of
 * Plucked-String and Drum Timbres", Computer Music Journal 7(2), 1983;
 * David Jaffe, Julius O. Smith, "Extensions of the Karplus-Strong
 * Plucked-String Algorithm", Computer Music Journal 7(2), 1983. *)

(* [period frequency]: p, the delay line's length (200 for 220 Hz) *)
val period : float -> int

(* [render ?decay ~frequency seconds]: the string plucked and ringing;
 * [decay], 0.996 by default, the loss at each pass on top of the
 * averaging's (1: none); its noise from the NES's LFSR, so the same
 * every time *)
val render : ?decay:float -> frequency:float -> float -> Signal.t
