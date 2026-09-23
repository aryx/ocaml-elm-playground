(* Drift: an analog oscillator never quite in tune (see notes_synth.md
 * section 3).
 *
 * A voltage-controlled oscillator's frequency depends on its
 * transistors' temperature: it wanders by a few cents as the circuit
 * warms and the room changes. Two digital sawtooths detuned by 2 cents
 * beat at a fixed rate (440 x (2^(2/1200) - 1) = 0.51 times a second at
 * A4), mechanically, forever; two drifting ones beat at a rate that
 * slowly changes, and the sound breathes. Part of what people call
 * analog "warmth" is this imprecision, and a virtual analog synthesizer
 * adds it back on purpose.
 *
 * The model: a random walk that is pulled back towards 0, so it wanders
 * without wandering off (an Ornstein-Uhlenbeck process, 1930, the
 * velocity of a particle in a fluid):
 *
 *     x' = x (1 - h / tau) + sigma sqrt h r
 *
 *     cents
 *      +3 |      .-.                 .--.
 *       0 |--.--'   '-.     .-.  .--'    '-.
 *      -3 |   '        '---'   ''           '--      tau: how long a
 *         +------------------------------------> t   wander lasts
 *
 * r a random number of variance 1 (Noise.uniform, stretched by sqrt 3),
 * h the time of a step, tau the time the pull takes. Its spread settles
 * at sigma sqrt (tau / 2), so sigma = cents sqrt (2 / tau) makes the
 * spread [cents]: at the default, 3 cents, the note within +- 3 cents
 * two thirds of the time, a frequency factor of 2^(3/1200) = 1.00173
 * (0.76 Hz at A4); over tau = 2 s, a wander.
 *
 * A step every 64 samples of the audio clock, whatever the blocks it is
 * advanced by: the same drift for a golden run's pulls of 735 as for
 * SDL's, so a drifting patch has golden WAVs too. From a seed: each
 * oscillator its own. *)

type t

(* [create ?cents ?seconds ~seed ()]: a drift of spread [cents]
 * (default 3), wandering over [seconds] (tau, default 2), from a value
 * drawn at random *)
val create : ?cents:float -> ?seconds:float -> seed:int -> unit -> t

(* [advance t n]: [n] samples later *)
val advance : t -> int -> unit

(* how far out of tune, now, in cents; as a factor for a frequency *)
val cents : t -> float
val factor : t -> float

(* the samples between two steps: 64 *)
val step_samples : int
