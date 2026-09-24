(* The DX7's envelope: four rates, four levels, in decibels (see
 * notes_synth.md; plan_synth_teaching.md, TinyDX7, D1).
 *
 * An ADSR (Envelope.mli) has an attack, a decay to a sustain, and a
 * release: three times and a level. Yamaha's is more general and
 * stranger: four *levels* L1 to L4 and four *rates* R1 to R4, each 0
 * to 99. The key pressed, the level goes to L1 at the speed R1, then to
 * L2 at R2, then to L3 at R3, and stays there while the key is held;
 * let go, it goes to L4 at R4:
 *
 *     99 |    L1
 *        |   /\
 *        |  /  \  L2
 *        | /    \______  L3 (held)
 *        |/            \
 *      0 +--------------\____ L4
 *         R1  R2  R3      R4
 *         key down        key up
 *
 * A *rate*, not a time: the speed is fixed, so the time depends on how
 * far the level goes -- a patch moved from L1 99 to 50 decays in half
 * the time. And every level can be anywhere, so the "decay" can rise
 * (a swell after the strike: L2 above L1), which an ADSR can't.
 *
 * It all happens in the *log domain*: the level counts in steps of
 * 6.02 / 256 dB (256 steps a doubling of the amplitude, Dexed's unit),
 * and a rate is a speed in steps per sample, doubling every 4 rates
 * (4 + qrate mod 4) x 2^(qrate / 4 + 2) / 65536, qrate = rate x 41 / 64,
 * plus the rate scaling (faster up the keyboard, Dx7_voice's). Falling,
 * the level moves by that much each sample: a straight line in
 * decibels, so an exponential in amplitude, as a real sound dies.
 * Rising, it moves by that times how many doublings remain below 17,
 * whole ones: fast from low, slowing near the top, a curve -- and from
 * below step 1716 (about -50 dB under full) it first *jumps* there, so
 * an attack never crawls through the inaudible: the DX7's snap.
 *
 *     steps  3840 |          ___.---      rising: slows near the top
 *                 |      .-''
 *            1716 |  ___|                  (jumped to at once)
 *              16 +-'--------------> t
 *
 * A level 0-99 becomes steps through `scale_output_level` (a table
 * below 20, then 28 + level, 0 to 127), times 32, plus the operator's
 * own output level (its level 0-99 scaled the same, times 32, plus the
 * keyboard's and the velocity's scaling) less 4256; at least 16. At
 * full (99 and 99), 3840 steps: 15 doublings.
 *
 * The operator's amplitude is then 2^(steps / 256 - 14) (`gain`), in
 * *cycles* of the phase it modulates: 2 at full, a modulation index of
 * 4 pi radians, the DX7's largest.
 *
 * Worked example (Unit_dx_envelope): L1 99 at R1 99 then L2 0 at R2
 * 50, full output level: the attack jumps to 1716 and reaches 3840 in
 * 33 samples, 0.75 ms; the decay falls 3824 steps at 1/16 a sample,
 * 61,184 samples, 1.387 s, a straight line: 64.8 dB lost after a
 * second. The same decay from L1 50 (78 x 32 + 4064 - 4256 = 2304
 * steps) falls 2288 steps at the same speed: 36,608 samples, a rate a
 * speed, not a time. Held at L3 80 then let go, R4 60: 17,323 samples
 * to silence.
 *
 * References: Yamaha DX7 Operating Manual (1983), "EG"; Raph Levien's
 * msfa (in Dexed, env.cc: the constants above, measured against the
 * hardware); Chowning and Bristow, FM Theory and Applications (1986). *)

(* [scale_output_level level]: a level 0-99 onto 0-127 *)
val scale_output_level : int -> int

(* 16 steps, the floor: silence *)
val floor : float

(* [gain steps]: the amplitude, in cycles, 2^(steps / 256 - 14) *)
val gain : float -> float

(* [decibels steps]: the level under full (3840 steps), in dB *)
val decibels : float -> float

type t

(* [create ~rates ~levels ?output_level ?rate_scaling ()]: the key
 * pressed, from silence. [output_level] in steps (the operator's level
 * scaled, times 32, plus its keyboard's and velocity's scaling), 4064
 * at full; [rate_scaling] added to every qrate, 0 by default *)
val create : rates:int array -> levels:int array -> ?output_level:int -> ?rate_scaling:int -> unit -> t

(* the key let go: to L4 at R4 *)
val key_up : t -> unit

(* [next t]: the level one sample on, in steps *)
val next : t -> float

(* [stage t]: 0 to 3 going to L1 to L4 (3 held until the key's let
 * go), 4 when L4 is reached *)
val stage : t -> int
