(* Screen shake: the whole picture knocked about for a moment when
   something big happens.

   Jan Willem Nijman's talk "The Art of Screen Shake" (2013, Vlambeer's
   Nuclear Throne) made it famous; Squirrel Eiserloh's "Juicing Your
   Cameras With Math" (GDC 2016) is where the two ideas of this module
   come from.

   What decays is trauma, not shake. A hit adds *trauma*, a number from
   0 (calm) to 1 (the worst), which falls back to 0 over time; the shake
   is trauma squared. Two small hits then barely move the screen and a
   big one really does, and the end of a shake fades out gently
   instead of stopping:

       trauma   shake = trauma^2   at most, with 40 pixels
        1.0         1.0              40 px
        0.5         0.25             10 px
        0.25        0.06              2.5 px

   The shake is read from noise, not drawn at random. The obvious shake
   is a new random offset every frame ([jitter] below, kept to compare):
   at 60 frames a second the picture jumps from anywhere to anywhere,
   which looks like a broken video signal, not like a camera knocked.
   Eiserloh's fix is smooth noise: random values at a slower rate (25
   a second here), and the shake gliding from one to the next along a
   smooth curve -- Ease.smoothstep, the same shape as Ken Perlin's
   interpolation:

      jitter (a new value each frame)     noise (25 a second, smoothed)
        |  .   .     .                      |      .--.
        | . . . .  .. .                     |   .-'    '.     .-.
        |.   .   ..    .                    |.-'         '---'   '-
        |     .      .                      |

   And noise is a function of the time: the same time, the same shake,
   so a replay and a golden frame see the same picture. Three noises of
   different seeds make the three offsets: across, up, and a slight
   turn.

   The random values themselves come from Hash.mli: a number from the
   seed and the lattice point, the same every time, natively and in a
   browser.

   Worked example (checked by the tests): [Hash.hash ~seed:1] is -0.1084
   at point 0 and 0.5113 at point 1, so [noise ~seed:1] is -0.1084 at 0,
   their mean 0.2014 at 0.5, and -0.0116 at 0.25 (smoothstep 0.25 =
   0.156 of the way). A trauma of 0.5 falls to 0.25 in a quarter of a
   second at the default rate, 1 a second.

   Honest about scale: 1D value noise, the simplest smooth noise; Perlin's
   gradient noise (1985) is smoother still, and matters for terrain,
   not for a shake that lasts half a second. *)

(* [add amount trauma]: after a hit, at most 1 *)
val add : float -> float -> float

(* [decay ?per_second ~dt trauma]: [dt] seconds later, falling by
 * [per_second] (1 by default) a second, at least 0 *)
val decay : ?per_second:float -> dt:float -> float -> float

(* [shake trauma]: trauma squared *)
val shake : float -> float

(* [noise ~seed x]: smooth, in [-1, 1]: [Hash.hash] at the whole numbers,
 * smoothstep between them *)
val noise : seed:int -> float -> float

(* [jitter ~seed x]: [Hash.hash] at the whole number below [x]: a new random
 * value at each whole number, the simple shake *)
val jitter : seed:int -> float -> float

type offset = { dx : float; dy : float; angle : float (* degrees *) }

(* [offset ?smooth ?max_offset ?max_angle ~seed ~trauma time]: where the
 * picture is at [time] (seconds), shaken by [trauma]: [shake trauma]
 * times [max_offset] (40 pixels) and [max_angle] (5 degrees), read
 * from [noise] at 25 points a second, or with [~smooth:false] from
 * [jitter] at 60, a new one each frame *)
val offset : ?smooth:bool -> ?max_offset:float -> ?max_angle:float -> seed:int -> trauma:float -> float -> offset
