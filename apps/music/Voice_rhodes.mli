(* The electric pianos: the Rhodes' tine, the Wurlitzer's reed, the
 * Clavinet's string, each through its pickup (see notes_synth.md
 * section 10; plan_synth_teaching.md, TinyRhodes, R1).
 *
 * The electric pianos of the 1960s and 70s are acoustic instruments
 * too quiet to hear -- a struck tine, reed or string -- made loud by a
 * pickup, and their sound is the pickup's as much as the vibration's.
 * *Physical modelling*: the sound computed from the parts that make
 * it, here with the fewest parts that give each its character.
 *
 * The Fender Rhodes (Mark I, 1970): a hammer with a neoprene tip
 * strikes a *tine*, a thin steel rod bolted to a brass tone bar, the
 * two a tuning fork. The tine is a clamped-free beam, whose modes sit
 * at 1, 6.27 and 17.55 times its fundamental (Euler and Bernoulli's
 * beam): three Modal resonators, the upper two gone in a moment (the
 * attack's "ping"), the fundamental ringing for seconds, the tone bar
 * keeping it. Harder, the hammer's shorter contact excites the upper
 * modes more. The tine's tip swings in front of an electromagnetic
 * pickup, and the voltage is the rate of change of the magnetic flux
 * (Faraday), the flux a bell-shaped function of where the tip is:
 *
 *     flux                        the tip's swing: small, on the
 *       |        .-'''-.          bell's side, nearly a sine out;
 *       |      .'   |   '.        across its top, the output folds:
 *       |    .'     |     '.      more harmonics (the "bark")
 *       | ..'       |       '..
 *       +-----------+-----------> tip's position
 *               offset (voicing)
 *
 * so the *voicing* -- how far the tine sits off the pickup's centre,
 * set with a screwdriver on the real one -- chooses the timbre: centred,
 * the swing crosses the top, mostly the second harmonic; off, the
 * fundamental and a lopsided curve, the Rhodes' tone. And the harder
 * the key, the wider the swing, the more of the curve it sweeps: the
 * bark, velocity changing the timbre, as FM does (Dx7_voice.mli), for
 * a physical reason.
 *
 * The Wurlitzer (200A, 1974): a steel *reed* struck, nearly one mode,
 * its pickup a capacitor -- the reed at 0 volts between the cutouts of
 * a plate at 170 -- whose capacitance varies inversely with the gap,
 * C = 1 / (1 - x) for the reed x of the gap closer; the current dC/dt.
 * Near the plate, the curve steepens on one side only: an asymmetric
 * distortion, even and odd harmonics, the Wurlitzer's reedy bite
 * (Supertramp, Ray Charles).
 *
 * The Clavinet (D6, 1971): a string struck against an anvil by a
 * rubber tip, a yarn damping it when the key comes up: Pluck.mli's
 * Karplus-Strong string, bright and short (Stevie Wonder's
 * "Superstition").
 *
 * The Suitcase's "vibrato" (the Rhodes' amplifier) is a stereo
 * tremolo: the sound moving between the two speakers, a sine's rate
 * and depth. The Wurlitzer's is its loudness only.
 *
 * Facts from Florian Pfeifle's model (DAFx 2017, measured with a
 * high-speed camera); ours, and said so: the bell's shape (a
 * Lorentzian, 1 / (1 + x^2)), how far the swing goes, the decay
 * times, the hammer's share of the upper modes.
 *
 * Worked example (Unit_rhodes), C4 0.2 s in: on the Rhodes, the second
 * harmonic against the fundamental -24.8 dB at velocity 0.2, +1.2 dB at
 * 1 (the harmonics 2 to 5 together -23.0 and +4.3: the bark); voicing
 * 0 against 1 at 0.5, +10.5 dB and -14.3 (near the centre, twice the
 * frequency); the Wurlitzer's harmonics 2 to 5 -25.8 and -11.5; the
 * Suitcase at depth 0.8, one side 3.3 times the other at its extremes;
 * released notes freed; the presets on a phrase peaking from 0.28 to
 * 0.90. The upper modes' motion is kept small: the pickup reads a rate
 * of change, so a mode 6.27 times higher is 6.27 times louder for the
 * same motion -- the first version, shares set as if it read the
 * position, peaked at 5.7. *)

type patch = {
  model : int; (* an index in [models] *)
  voicing : float; (* 0 to 1: the tine off the pickup's centre, the reed nearer its plate *)
  hardness : float; (* 0 to 1: the hammer, the upper modes' share *)
  decay : float; (* 0 to 1: how long a note rings *)
  tremolo_rate : float; (* 0 to 1: 1 to 10 Hz *)
  tremolo_depth : float; (* 0 to 1 *)
  volume : float;
}

val models : string list
val initial : patch

type knob = patch Patch_text.knob

val knobs : knob list
val to_string : patch -> string
val of_string : string -> (patch, string) result

(* ours: Mark I, bark, suitcase, wurlitzer, clavinet *)
val presets : (string * patch) list

(* the tine's modes, as ratios of its fundamental *)
val tine_modes : float list

(* [pickup ~voicing x]: the Rhodes' flux at the tip's position [x] (in
 * the field's widths); [capacitance ~voicing x] the Wurlitzer's *)
val pickup : voicing:float -> float -> float
val capacitance : float -> float

type t

val create : patch -> t
val patch : t -> patch
val set_patch : t -> patch -> unit
val voices : t -> int

(* the last 2048 samples played, left, for a scope *)
val recent : t -> Signal.t

(* the tremolo's position, -1 (left) to 1 (right), for a panel *)
val pan : t -> float
val instrument : t -> Instrument.t
