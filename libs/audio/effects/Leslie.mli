(* The Leslie speaker: a sound turned around, the Hammond's other half
 * (see notes_synth.md; plan_synth_teaching.md, TinyHammond, H3).
 *
 * Don Leslie's cabinet (1941; the 122 of 1965 the Hammond's usual
 * partner) makes no sound of its own: it moves the organ's. A crossover
 * splits the sound at 800 Hz, the highs to a horn and the lows to a
 * drum, and a motor turns both, the other way from each other:
 *
 *               listener
 *                  ^
 *          .-------|-------.
 *          |  <-- horn --> |   the horn's mouth on a circle of 15 cm
 *          |   (rotates)   |   (ours), coming towards the listener,
 *          |               |   then going away
 *          |  [ drum  ]    |   the drum a baffle on the woofer, its
 *          '---------------'   opening turning the other way
 *
 * Two things happen at once, from one cause:
 *
 *  - a *Doppler shift*: the horn's mouth coming nearer shortens the
 *    path, the sound arrives compressed, higher; going away, lower. The
 *    distance to the listener, d(t) = D - r cos (2 pi f t), read as a
 *    delay, d(t) / 343 m/s: a delay line read at a moving delay makes
 *    the shift itself (Modulated_delay.mli's chorus is the same
 *    mechanism, its delay moved by an LFO, here by geometry). The
 *    swing, v / c with v = 2 pi r f: at tremolo, the horn's mouth at
 *    2 pi x 0.15 x 6.8 = 6.4 m/s, 1.87%, +-32 cents;
 *  - a *tremolo*: the horn's mouth facing the listener is loud,
 *    turned away quieter (0.6 + 0.4 cos, ours).
 *
 * Two microphones, left and right of the cabinet (a quarter turn each
 * side), hear each at its own angle: the rotation sweeps across the
 * stereo field.
 *
 * The speeds (the 122's): the horn 0.8 turns a second (chorale, slow)
 * or 6.8 (tremolo, fast), the drum 0.7 or 5.6; switched, each speeds up
 * or slows down as a heavy thing does, the horn in 0.5 s (a time
 * constant: 63% of the way), the drum, heavier, in 1.2 s -- the ramp
 * between the speeds, the horn arriving first, is the Leslie's most
 * played sound.
 *
 * Worked example (Unit_leslie): switched to fast, the horn at 4.59
 * turns a second after 0.5 s (0.8 + 63% of 6.0), the drum at 3.80
 * after 1.2 s (0.7 + 63% of 4.9); switched back, the horn at 3.01
 * after 0.5 s. A 5 kHz sine at full speed (the horn's alone: the drum's
 * low-pass lets it through 32 dB under), its frequency in 5 ms windows
 * swinging between 5093 and 4907 Hz (5000 x (1 +- 0.0187)), its level
 * between 1.017 (facing, the drum's faint copy adding) and 0.20
 * (turned away: 0.6 - 0.4). The two microphones hear different
 * samples.
 *
 * References: Clifford A. Henricksen, "Unearthing the Mysteries of the
 * Leslie Cabinet", Recording Engineer/Producer, 1981; Julius O. Smith
 * III et al., "Doppler Simulation and the Leslie", DAFx 2002; the 122's
 * speeds and crossover as Hammond's manual and the Leslie's entry in
 * Wikipedia give them. *)

type t

val create : unit -> t

(* [process t ~fast s]: [s] (its two sides mixed) through the cabinet,
 * heard by the two microphones, in place; [fast] the switch: tremolo,
 * else chorale *)
val process : t -> fast:bool -> Signal.stereo -> unit

(* the rotors' speeds now, turns a second *)
val horn : t -> float
val drum : t -> float

(* the speeds and time constants: chorale and tremolo, horn and drum *)
val horn_slow : float
val horn_fast : float
val drum_slow : float
val drum_fast : float
val horn_seconds : float
val drum_seconds : float

(*****************************************************************************)
(* {1 As an effect} *)
(*****************************************************************************)

(* fast (a switch: tremolo, else chorale), mix (0 to 1: the Leslie's
 * share, 1 at first) *)
val knobs : Effect.knob list

(* [effect ()]: "leslie", its meters the rotors' speeds, "horn" and
 * "drum" (turns a second) *)
val effect : unit -> Effect.t
