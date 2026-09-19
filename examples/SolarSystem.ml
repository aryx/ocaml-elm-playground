(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The Solar System seen from above, from 2000-01-01 on: an orrery (the
 * clockwork models of the planets, named after the Earl of Orrery, for
 * whom John Rowley built one around 1713, after George Graham's), the
 * planets where they really were and will be, on their real ellipses,
 * at their real speeds, sped up.
 *
 *   up/down  faster/slower (days per second)     space  pause
 *   d        true distances / compressed          r      back to 2000
 *   s        true sizes / magnified
 *
 * Design notes:
 *
 * - Where the planets are comes from Kepler's laws in closed form
 *   (physics/2d/Kepler.mli: Kepler's equation, solved by Newton's
 *   method), with JPL's orbital elements at J2000 (Planets.mli), not
 *   from integrating the Sun's gravity step by step like
 *   examples/Orbit.ml: at 20 days a second, a 1/60 s frame is a third
 *   of a day, and a year per second (up) is 6 days a frame, too coarse
 *   for Mercury's 88-day orbit; the closed form is exact at any date.
 *   Mercury's orbit is visibly off-center (e = 0.21): the Sun is at a
 *   focus, not the center (Kepler's first law), and it goes faster
 *   near the Sun (the second).
 *
 * - The scale, the problem every picture of the Solar System has
 *   (Planets.mli): with Neptune at the screen's edge (30 AU), the Earth
 *   would be 15 pixels from the Sun and 0.0006 pixels across. So two
 *   lies, each switchable to see the truth:
 *   - distances compressed: a planet r AU from the Sun is drawn
 *     78 sqrt r pixels from it (the angles kept, so the ellipses stay
 *     ellipses, a little squashed); d: true, 15 pixels per AU, the
 *     inner planets crowded around the Sun;
 *   - sizes magnified: a planet of radius R is drawn 2 + 3 sqrt (R /
 *     R_earth) pixels (the Earth 5, Jupiter 12), the Sun 20; s: true,
 *     at the distances' scale -- nothing left to see but the labels.
 *   The Moon's orbit is magnified the same way (12 pixels from the
 *   Earth: its true 0.00257 AU would be inside the Earth's disk).
 *
 * - The planets move in 3D (Mercury's orbit is tilted 7 degrees): seen
 *   from above, the z is dropped; examples3d/SolarSystem3d.ml keeps it.
 *
 * - Left out: the Moon's real phase and tilt (it starts at an
 *   arbitrary place), the planets' pulls on each other (the elements'
 *   drift per century in JPL's table), the moons of the others,
 *   Pluto (a dwarf planet since 2006).
 *
 * References: Kepler, Astronomia Nova (1609) and Harmonices Mundi
 * (1619); Jean Meeus, Astronomical Algorithms (1991); E. M. Standish,
 * J. G. Williams, "Approximate Positions of the Planets", JPL,
 * https://ssd.jpl.nasa.gov/planets/approx_pos.html; NASA's Planetary
 * Fact Sheet, https://nssdc.gsfc.nasa.gov/planetary/factsheet/; the
 * date from a day count: Howard Hinnant's civil_from_days,
 * http://howardhinnant.github.io/date_algorithms.html; the orrery:
 * https://en.wikipedia.org/wiki/Orrery
 *
 * What it uses: the Playground, Scene2d (the keys pressed), and from the
 * engine (physics/2d/) Kepler and Planets only: no Physics layer, no
 * forces, no collisions.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state = {
  (* days since J2000, 2000-01-01 at noon *)
  days : number;
  (* days per second *)
  speed : number;
  paused : bool;
  true_distances : bool;
  true_sizes : bool;
}

type model = state Scene2d.t

let initial_model : model =
  Scene2d.start { days = 0.; speed = 20.; paused = false; true_distances = false; true_sizes = false }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene in
  let pressed k = Scene2d.pressed k scenes and letter l (k : keyboard) = Set_.mem l k.keys in
  let s =
    {
      s with
      speed =
        (if pressed (fun k -> k.kup) then min 3650. (s.speed * 2.)
         else if pressed (fun k -> k.kdown) then max 0.25 (s.speed / 2.)
         else s.speed);
      paused = (if pressed (fun k -> k.kspace) then not s.paused else s.paused);
      true_distances = (if pressed (letter "d") then not s.true_distances else s.true_distances);
      true_sizes = (if pressed (letter "s") then not s.true_sizes else s.true_sizes);
    }
  in
  let s = if pressed (letter "r") then { s with days = 0. } else s in
  (* one frame, 1/60 s, fixed: the same frames for the same keys *)
  let s = if s.paused then s else { s with days = s.days + (s.speed / 60.) } in
  { scenes with scene = s }

(*****************************************************************************)
(* The scale *)
(*****************************************************************************)

(* pixels per AU at 1 AU, compressed; and true (Neptune's 30 AU at 450) *)
let compressed = 78.
let true_scale = 15.

(* a point r AU from the Sun: its distance on the screen *)
let screen_distance (s : state) (r : number) : number = if s.true_distances then true_scale * r else compressed * sqrt r

(* x, y in AU (seen from above) to the screen *)
let to_screen (s : state) ((x, y) : number * number) : number * number =
  let r = Float.hypot x y in
  if r = 0. then (0., 0.) else let k = screen_distance s r / r in (x * k, y * k)

let earth_radius = 6378.

(* a body of radius [km] on the screen *)
let size (s : state) (km : number) : number =
  if s.true_sizes then km / Planets.au * (if s.true_distances then true_scale else compressed)
  else 2. + (3. * sqrt (km / earth_radius))

let sun_size (s : state) : number = if s.true_sizes then Planets.sun_radius / Planets.au * (if s.true_distances then true_scale else compressed) else 20.

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let color_of (name : string) : color =
  match name with
  | "Mercury" -> rgb 160 160 160
  | "Venus" -> rgb 230 200 140
  | "Earth" -> rgb 70 130 230
  | "Mars" -> rgb 210 90 50
  | "Jupiter" -> rgb 210 170 120
  | "Saturn" -> rgb 230 210 150
  | "Uranus" -> rgb 150 210 230
  | _ -> rgb 80 110 220

let position (p : Planets.planet) (days : number) : number * number =
  let (x, y, _) = Kepler.position p.orbit ~days in
  (x, y)

(* an orbit: 120 dots along one period *)
let orbit (s : state) (p : Planets.planet) : shape list =
  let period = Kepler.period p.orbit.a * 365.25 in
  List.init 120 (fun i ->
      let (x, y) = to_screen s (position p (float_of_int i * period / 120.)) in
      circle (rgb 60 60 80) 1. |> move x y)

(* the night sky: dots from a linear congruential generator (the same
 * every time, no Random) *)
let stars : shape list =
  let rec go n seed acc =
    if n = 0 then acc
    else
      let seed = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff in
      let x = float_of_int (seed mod 1000) - 500. and y = float_of_int (seed /.. 1000 mod 1000) - 500. in
      go (n -.. 1) seed ((circle (rgb 120 120 120) 0.8 |> move x y) :: acc)
  in
  go 150 11 []

(* days since J2000 to a date, by Howard Hinnant's civil_from_days (days
 * since 1970-01-01; J2000's day is 10957) *)
let date (days : number) : string =
  let z = 10957 +.. int_of_float (Float.floor (days + 0.5)) +.. 719468 in
  let era = (if z >= 0 then z else z -.. 146096) /.. 146097 in
  let doe = z -.. (era *.. 146097) in
  let yoe = (doe -.. (doe /.. 1460) +.. (doe /.. 36524) -.. (doe /.. 146096)) /.. 365 in
  let doy = doe -.. ((365 *.. yoe) +.. (yoe /.. 4) -.. (yoe /.. 100)) in
  let mp = ((5 *.. doy) +.. 2) /.. 153 in
  let d = doy -.. (((153 *.. mp) +.. 2) /.. 5) +.. 1 in
  let m = if mp < 10 then mp +.. 3 else mp -.. 9 in
  let y = yoe +.. (era *.. 400) +.. if m <= 2 then 1 else 0 in
  Printf.sprintf "%04d-%02d-%02d" y m d

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let view_planet (s : state) (p : Planets.planet) : shape list =
  let (x, y) = to_screen s (position p s.days) in
  let r = size s p.radius in
  let color = color_of p.name in
  (* Saturn's rings, seen from above: a disk behind it, see-through *)
  let rings = if p.name = "Saturn" then [ circle (rgb 200 190 150) (r * 2.1) |> fade 0.5 |> move x y ] else [] in
  (* the Moon around the Earth *)
  let moon =
    if p.name = "Earth" then
      let a = 2. * Float.pi * s.days / Planets.moon_period in
      let d = if s.true_sizes then Planets.moon_distance / Planets.au * (if s.true_distances then true_scale else compressed) else 12. in
      [ circle (rgb 200 200 200) (size s Planets.moon_radius) |> move (x + (d * cos a)) (y + (d * sin a)) ]
    else []
  in
  rings @ [ circle color r |> move x y ] @ moon @ [ text (rgb 180 180 180) 1.3 p.name |> move (x + 8.) (y + 12.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and s = model.scene in
  (rectangle black screen.width screen.height :: stars)
  @ List.concat_map (orbit s) Planets.planets
  @ [ circle (rgb 255 200 60) (sun_size s); circle (rgb 255 240 150) (sun_size s * 0.7) ]
  @ List.concat_map (view_planet s) Planets.planets
  @ [ text white 3. (date s.days) |> move (-340.) 450.;
      text (rgb 200 200 200) 2. (Printf.sprintf "%g days a second%s" s.speed (if s.paused then " (paused)" else "")) |> move (-340.) 415.;
      text (rgb 200 200 200) 1.6
        (Printf.sprintf "distances: %s (d)   sizes: %s (s)"
           (if s.true_distances then "true" else "compressed")
           (if s.true_sizes then "true, too small to see" else "magnified"))
      |> move 0. (-450.);
      text (rgb 150 150 150) 1.4 "up/down: faster/slower   space: pause   r: back to 2000" |> move 0. (-475.) ]

let help =
  {|SolarSystem
  keys:  up/down  faster/slower        space  pause
         d        true distances / compressed
         s        true sizes / magnified
         r        back to 2000-01-01
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
