(* The Hammond organ's tonewheels: 91 sines made by gears (see
 * notes_synth.md; plan_synth_teaching.md, TinyHammond).
 *
 * Laurens Hammond's organ (1935) makes its sines mechanically: a motor
 * turns a shaft at 20 revolutions a second (1200 rpm, locked to the
 * mains' frequency), and on it, through gears, 91 steel wheels, each
 * with bumps on its edge spinning past a magnet and a coil: a wheel
 * with n bumps turning r times a second induces a sine of n r hertz.
 * A key doesn't start anything: every wheel turns all the time, and a
 * key only connects some of them to the output.
 *
 * The notes of an octave come from 12 pairs of gears, driving/driven,
 * as close as whole teeth get to the twelfth root of 2:
 *
 *     C 85/104   C# 71/82   D 67/73   D# 105/108   E 103/100   F 84/77
 *     F# 74/64   G 98/80    G# 96/74  A 88/64      A# 67/46    B 108/70
 *
 * and the octaves from the bumps: 2 on the lowest octave's wheels, 4, 8,
 * ..., 128 on the seventh's (wheels 1 to 84, C1 to B7); the last seven
 * (C8 to F#8) have 192, on F's to B's gears. So A, 88/64 = 11/8, is
 * exact: A4 = 20 x 11/8 x 16 = 440 Hz; the other notes miss equal
 * temperament by less than a cent, and the top seven are not exact
 * octaves of the ones below.
 *
 * Worked example (Unit_hammond): A4 440 Hz exactly; C4 (wheel 37)
 * 20 x 85/104 x 16 = 261.538 Hz, 0.58 cents flat of equal temperament;
 * the top wheel, F#8, 20 x 108/70 x 192 = 5924.57 Hz. And the
 * drawbars' "harmonics" are these wheels: the 2 2/3' of C4 (its third
 * harmonic, an octave and a fifth up) is G5's wheel, 20 x 98/80 x 32 =
 * 784.0 Hz, where the true third harmonic is 3 x 261.538 = 784.62: 1.36
 * cents flat. A real
 * harmonic would be locked to its note; this one beats against it, a
 * slow shimmer that is part of the Hammond's sound (a tempered
 * harmonic).
 *
 * References: the Hammond B-3's service manual; HammondWiki, "Gear
 * Ratio", https://www.dairiki.org/HammondWiki/GearRatio; goodeveca's
 * "Hammond Tone Wheels",
 * https://www.goodeveca.net/RotorOrgan/ToneWheelSpec.html. *)

(* 91 *)
val count : int

(* the gears, driving and driven teeth, C to B *)
val gears : (int * int) array

(* [frequency w]: wheel [w]'s, 1 to [count] *)
val frequency : int -> float

(* [of_note n]: the wheel of MIDI note [n] (C1, 24, is wheel 1; F#8,
 * 114, wheel 91), a note beyond folded back by octaves into the wheels
 * there are (the drawbars' foldback: the 16' of the lowest keys, the 1'
 * of the highest) *)
val of_note : int -> int

(* [cents w]: wheel [w]'s distance from equal temperament (A4 = 440),
 * in cents *)
val cents : int -> float
