(* Sound in space: where a sound comes from, heard with two ears (see
 * notes_audio.md section 5, "Two ears").
 *
 * The ear finds a sound's direction mostly from two differences
 * between the ears: the level (the head shadows the far ear) and the
 * time (the far ear hears it later, by up to 0.66 ms). Stereo speakers
 * or headphones can give both; here, the level (panning), and the time
 * for the sounds played once (below).
 *
 * {2 The pan law}
 *
 * A pan p from -1 (left) to 1 (right) gives each channel a gain. The
 * obvious way, linear, left 1 - p and right 1 + p, has a hole in the
 * middle: the loudness we hear is the power, the sum of the squares, 2
 * in the middle (1 and 1) but 4 at a side (2 and 0): a sound crossing
 * from left to right dips by 3 dB as it passes the centre. The constant
 * power law keeps the sum of the squares the same everywhere, on a
 * quarter circle:
 *
 *     angle = (p + 1) pi / 4        left = sqrt 2 cos angle
 *                                   right = sqrt 2 sin angle
 *
 *     p        -1     -0.5    0      0.5    1
 *     left     1.41   1.31    1      0.54   0
 *     right    0      0.54    1      1.31   1.41       l^2 + r^2 = 2
 *
 * (sqrt 2 so that the middle is 1 and 1: a sound not panned, as every
 * sound was before stereo, is a sound in the middle, as loud as it
 * was.) Alan Blumlein's stereo patent (EMI, 1931) had already panned
 * with two gains; the constant power law is the mixing desk's.
 *
 * {2 The time between the ears}
 *
 * A sound from the side reaches the far ear later: around the head, a
 * sphere of radius r = 8.75 cm, the path is longer by r (theta + sin
 * theta) at an angle theta from straight ahead (Robert Woodworth,
 * 1938): at most (pi/2 + 1) r / c = 0.656 ms, 29 samples, for a sound
 * straight to the side. [ears_apart]: the far ear's channel delayed by
 * that, for a sound played once (Synth.render_stereo); a continuous
 * sound, its pan changing every frame, keeps to the level (a delay that
 * moves needs a fractional delay line, or it clicks: an exercise).
 *
 * {2 Distance}
 *
 * A sound spreads over a sphere, its intensity falling as 1 / d^2 and
 * its amplitude as 1 / d: half as loud (-6.02 dB) each time the
 * distance doubles, the inverse distance law. Up to a [reference]
 * distance it stays at full volume (closer than that, 1 / d would grow
 * without bound): OpenAL's "inverse distance clamped" model.
 *
 * Air also absorbs, the high frequencies much more than the low: a far
 * thunder is a rumble. The standard's table (ISO 9613-1, at 20 degrees
 * and 70% humidity) gives about 0.023 dB a meter at 4 kHz and 0.077 at 8
 * kHz; a power law through those two, 0.0766 (f / 8000)^1.75 dB a
 * meter, is close above 4 kHz (below, it says too little: 2.0 dB a km
 * at 1 kHz for the table's 5.0 -- but there it hardly matters short of
 * a kilometer). [air_cutoff d]: where that loss reaches 3 dB, the
 * cutoff of a low-pass standing for the air: 7.9 kHz at 40 m, 4.0 kHz
 * at 130 m, 20 kHz (nothing) within 7.9 m.
 *
 * {2 Doppler}
 *
 * A source coming towards you squeezes its waves together, a higher
 * pitch; going away, a lower one (Christian Doppler, 1842; tested with
 * trumpeters on a train, Buys Ballot, 1845). With c the speed of sound,
 * and each speed taken along the line from the source to the listener
 * (towards the listener positive):
 *
 *     f' = f (c - v_listener) / (c - v_source)
 *
 * (OpenAL 1.1's formula: v_listener > 0, the listener going away from
 * the source; v_source > 0, the source coming.) Example, a car at 30
 * m/s (108 km/h), c = 343 m/s: coming, 343 / 313 = 1.096; going, 343 /
 * 373 = 0.920; passing, the pitch drops by their ratio, 1.19, 3.0
 * semitones: the "neeee-owww". The speeds are clamped below c: at c,
 * the waves pile up into a sonic boom, which this formula can't say.
 *
 * References: Alan Blumlein, British patent 394325, 1931; OpenAL 1.1
 * Specification, Creative Labs, 2005, sections 3.4 (distance models)
 * and 3.5.2 (velocity and Doppler); Christian Doppler, "Über das
 * farbige Licht der Doppelsterne", 1842; Jens Blauert, Spatial Hearing,
 * MIT Press, 1997 (the two ears' cues); Robert S. Woodworth,
 * Experimental Psychology, 1938 (the time between the ears); ISO
 * 9613-1:1993, Attenuation of sound during propagation outdoors, part
 * 1: calculation of the absorption of sound by the atmosphere. *)

(*****************************************************************************)
(* {1 Panning} *)
(*****************************************************************************)

(* [pan p]: the left and the right gains, the constant power law *)
val pan : float -> float * float

(* [pan_linear p]: 1 - p and 1 + p, the hole in the middle *)
val pan_linear : float -> float * float

(* true: the far ear's delay applied (see above); false: the level
 * only, as a pan knob does *)
val ears_apart : bool ref

(* [interaural_delay p]: the far ear's delay, in samples, for a sound
 * panned [p] (the sine of its angle): 0 in the middle, 29 at a side *)
val interaural_delay : float -> int

(*****************************************************************************)
(* {1 Positions} *)
(*****************************************************************************)

type vec = { x : float; y : float; z : float }

val vec : float -> float -> float -> vec

(* [direction ~listener ~right source]: the pan of a sound at [source]
 * for a listener at [listener] whose right is [right] (a unit vector):
 * the sine of the angle from straight ahead, 1 to the right, -1 to the
 * left, 0 ahead or behind (panning alone can't tell front from back);
 * 0 at the listener *)
val direction : listener:vec -> right:vec -> vec -> float

(* [distance a b] *)
val distance : vec -> vec -> float

(* [attenuation ~reference d]: 1 up to [reference], then reference / d *)
val attenuation : reference:float -> float -> float

(* [air_loss ~frequency d]: what [d] meters of air take from [frequency]
 * Hz, in dB (the power law above) *)
val air_loss : frequency:float -> float -> float

(* [air_cutoff d]: the frequency that has lost 3 dB over [d] meters,
 * at most 20 kHz *)
val air_cutoff : float -> float

(* [doppler ~speed_of_sound ~listener ~listener_velocity ~source
 * ~source_velocity]: f' / f, the formula above *)
val doppler :
  speed_of_sound:float -> listener:vec -> listener_velocity:vec -> source:vec -> source_velocity:vec -> float
