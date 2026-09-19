(* Mixing: sounds at once, and not too loud (see notes_audio.md section
 * 5).
 *
 * Two sounds at once are simply added, sample by sample, the way air
 * pressures add up. The catch is the range: two sines at full volume
 * add up to 2, outside the -1 to 1 a sound card takes, and the peaks
 * are cut flat -- clipping, a harsh distortion (the flat tops have
 * sharp corners: new, high harmonics). The fixes: turn each sound
 * down (the mixer's gain), or bend the curve smoothly near the limits
 * instead of cutting it, soft clipping (tanh here), which distorts
 * gently, like an overdriven tube amplifier:
 *
 *     out                        hard: min 1 (max -1 x)
 *      1 |      .-------         soft: tanh x
 *        |    ,'  _.----
 *        |   / .-'                2 (two full sines' peak): hard 1,
 *      0 +--/-'---------> in         soft tanh 2 = 0.964
 *          0    1    2
 *
 * Loudness in decibels, a logarithm: 20 log10 of the amplitude's ratio.
 * Half the amplitude is -6.02 dB, a tenth -20 dB, a hundredth -40 dB.
 * The ear hears ratios, not differences (the Weber-Fechner law), so a
 * volume in dB feels even, and one in plain amplitude doesn't: most
 * of a linear slider's travel sounds loud.
 *
 * References: Curtis Roads, The Computer Music Tutorial, 1996, chapter
 * 2; the bel after Alexander Graham Bell (Bell Labs, 1920s). *)

(* [add sounds]: summed, as long as the longest (the shorter ones
 * silent after their end) *)
val add : Signal.t list -> Signal.t

(* [gain g s]: each sample times [g] *)
val gain : float -> Signal.t -> Signal.t

(* [delay seconds s]: [s] starting [seconds] later, silence before *)
val delay : float -> Signal.t -> Signal.t

(* [then_ a b]: [b] right after [a] *)
val then_ : Signal.t -> Signal.t -> Signal.t

(* [decibels ratio]: 20 log10 ratio: -6.02 for 0.5 *)
val decibels : float -> float

(* [of_decibels db]: the amplitude ratio: 0.1 for -20 *)
val of_decibels : float -> float

(* one sample kept within [-1, 1]: cut flat, or bent by tanh *)
val clip : float -> float
val soft_clip : float -> float

(* [limit ?soft s]: every sample clipped, hard by default *)
val limit : ?soft:bool -> Signal.t -> Signal.t
