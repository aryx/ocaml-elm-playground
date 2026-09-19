(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The Solar System in 3D, from 2000-01-01 on: examples/PhysicsSolarSystem.ml
 * seen from the side, the planets on their real, tilted ellipses
 * (Mercury's 7 degrees), their night sides turned away from the Sun,
 * Saturn's rings tilted, the camera turning around it all.
 *
 *   left/right  turn the camera        up/down  nearer/farther
 *   w/s         faster/slower          space    pause
 *   r           back to 2000
 *
 * Design notes:
 *
 * - The orbits: Kepler's laws in closed form, with JPL's elements
 *   (physics/2d/Kepler.mli, Planets.mli), as in the 2D version (its
 *   header says why not integrate). Kepler.position gives the
 *   ecliptic's (x, y, z), z to its north; the playground's y is up:
 *   (x, y, z) is drawn at (x, z, -y), so that seen from above the
 *   planets turn counterclockwise, like in the 2D version.
 *
 * - The scale: compressed and magnified, as in 2D (3 sqrt r units for r
 *   AU, keeping the directions, so the tilts stay real; the planets
 *   0.12 + 0.18 sqrt (R / R_earth) units, the Earth 0.3, Jupiter 0.7),
 *   without 2D's keys to switch to the truth: in 3D, the true sizes
 *   are nothing to look at.
 *
 * - The light, the problem to simulate: the playground's 3D lighting
 *   is a fixed "sun", up and to the side, the same direction for every
 *   face (Playground3d.shading): with it, every planet would be lit
 *   from the same side, whatever the real Sun's direction. A point
 *   light at the Sun would need every backend's lighting changed (the
 *   software rasterizer's, OpenGL's and WebGL's shaders, the SVG one's
 *   flat colors). Instead: no lighting at all (No_lighting: every face
 *   its own color), and each planet wears a night side, a half-shell
 *   slightly bigger than it, its color darkened, turned away from the
 *   Sun: the phases (a crescent Venus when it's between us and the Sun)
 *   come out right, on every backend. Opaque, not a translucent shade:
 *   the software backend doesn't do alpha (fade3d), and the others then
 *   look the same. A sharp terminator (the day-night line), where a
 *   real light fades gradually: the price of the trick.
 *
 * - The background: the backends clear to white or leave it
 *   transparent; space is black, so the whole system sits in a big
 *   black box whose faces turn inwards (drawn from inside, with
 *   backface culling on), stars dotted on its walls.
 *
 * - The orbits are ribbons, 60 flat quads each, facing up (the camera
 *   stays above the ecliptic, so one side is enough), and never change:
 *   [cached3d], built once, the GPU backends keeping them in buffers
 *   (Playground3d.cached3d). The names are HUD words at the planets'
 *   projections (Playground3d.project).
 *
 * References: as the 2D version's -- Kepler; Meeus, Astronomical
 * Algorithms (1991); JPL's "Approximate Positions of the Planets",
 * https://ssd.jpl.nasa.gov/planets/approx_pos.html; NASA's Planetary
 * Fact Sheet, https://nssdc.gsfc.nasa.gov/planetary/factsheet/; Saturn's
 * axial tilt, 26.7 degrees, from the same fact sheet's Saturn page.
 *
 * What it uses: Playground3d (sphere, polygon3d, cached3d, hud,
 * project), Camera3d.orbit, Scene2d for the keys, and from the engine
 * Kepler and Planets only.
 *)
open Playground
open Playground3d
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state = {
  days : number;
  speed : number;
  paused : bool;
  (* the camera: its angle around the Sun (degrees), its distance *)
  turn : number;
  distance : number;
}

type model = state Scene2d.t

let initial_model : model = Scene2d.start { days = 0.; speed = 20.; paused = false; turn = 30.; distance = 26. }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene and k = computer.keyboard in
  let pressed f = Scene2d.pressed f scenes and letter l (k : keyboard) = Set_.mem l k.keys in
  let s =
    {
      s with
      speed =
        (if pressed (letter "w") then min 3650. (s.speed * 2.)
         else if pressed (letter "s") then max 0.25 (s.speed / 2.)
         else s.speed);
      paused = (if pressed (fun k -> k.kspace) then not s.paused else s.paused);
      (* turning by itself, 3 degrees a second, and by the arrows *)
      turn = s.turn + 0.05 + ((if k.kleft then 1.5 else 0.) - if k.kright then 1.5 else 0.);
      distance = max 6. (min 60. (s.distance + ((if k.kdown then 0.3 else 0.) - if k.kup then 0.3 else 0.)));
    }
  in
  let s = if pressed (letter "r") then { s with days = 0. } else s in
  let s = if s.paused then s else { s with days = s.days + (s.speed / 60.) } in
  { scenes with scene = s }

(*****************************************************************************)
(* The scale and the shapes *)
(*****************************************************************************)

(* the ecliptic's (x, y, z) in AU, to the scene: compressed, y up *)
let to_scene ((x, y, z) : number * number * number) : number * number * number =
  let r = sqrt ((x * x) + (y * y) + (z * z)) in
  if r = 0. then (0., 0., 0.) else let k = 3. * sqrt r / r in (x * k, z * k, -.y * k)

let size (km : number) : number = 0.12 + (0.18 * sqrt (km / 6378.))
let sun_size = 0.9

let color_of (name : string) : int * int * int =
  match name with
  | "Mercury" -> (160, 160, 160)
  | "Venus" -> (230, 200, 140)
  | "Earth" -> (70, 130, 230)
  | "Mars" -> (210, 90, 50)
  | "Jupiter" -> (210, 170, 120)
  | "Saturn" -> (230, 210, 150)
  | "Uranus" -> (150, 210, 230)
  | _ -> (80, 110, 220)

let lit ((r, g, b) : int * int * int) : color = rgb r g b
let night ((r, g, b) : int * int * int) : color = rgb (r /.. 6) (g /.. 6) (b /.. 6)

(* a point on the unit sphere, [phi] from the +x axis, [lambda] around
 * it *)
let dir (phi : number) (lambda : number) : number * number * number = (cos phi, sin phi * cos lambda, sin phi * sin lambda)
let scaled (k : number) ((x, y, z) : number * number * number) = (x * k, y * k, z * k)

(* the night side: the half-shell x >= 0, radius [r], 4 rings of 12
 * quads; turned away from the Sun by the caller *)
let half_shell (color : color) (r : number) : shape3d =
  let rings = 4 and around = 12 in
  let phi i = Float.pi / 2. * float_of_int i / float_of_int rings in
  let lambda j = 2. * Float.pi * float_of_int j / float_of_int around in
  group3d
    (List.concat
       (List.init rings (fun i ->
            List.init around (fun j ->
                polygon3d color
                  (List.map (scaled r)
                     [ dir (phi i) (lambda j); dir (phi (i +.. 1)) (lambda j); dir (phi (i +.. 1)) (lambda (j +.. 1)); dir (phi i) (lambda (j +.. 1)) ])))))

(* a planet at [pos], its night side turned away from the Sun (at the
 * origin): the half-shell's +x axis along [pos], turned about y (the
 * orbits are nearly flat: the few degrees of tilt left out of the
 * shade's direction) *)
let planet_shape (rgb3 : int * int * int) (r : number) ((x, y, z) : number * number * number) : shape3d =
  (* rotate3d turns +x about y by a to (cos a, 0, -sin a) *)
  let a = Float.atan2 (-.z) x * 180. / Float.pi in
  group3d [ sphere (lit rgb3) r; half_shell (night rgb3) (r * 1.04) |> rotate3d 0. a 0. ] |> move3d x y z

(* Saturn's rings: an annulus of 36 quads, both sides, tilted 26.7
 * degrees *)
let rings (r : number) : shape3d =
  let n = 36 in
  let point radius j = let t = 2. * Float.pi * float_of_int j / float_of_int n in (radius * cos t, 0., radius * sin t) in
  let color = rgb 200 185 140 in
  group3d
    (List.concat
       (List.init n (fun j ->
            let quad = [ point (r * 1.3) j; point (r * 2.2) j; point (r * 2.2) (j +.. 1); point (r * 1.3) (j +.. 1) ] in
            [ polygon3d color quad; polygon3d color (List.rev quad) ])))
  |> rotate3d 26.7 0. 0.

(* an orbit: a ribbon of 60 flat quads, facing up, 0.04 wide *)
let orbit_ribbon (p : Planets.planet) : shape3d =
  let period = Kepler.period p.orbit.a * 365.25 in
  let at i = to_scene (Kepler.position p.orbit ~days:(float_of_int i * period / 60.)) in
  let side (x, y, z) k = let r = Float.hypot x z in (x + (x / r * k), y, z + (z / r * k)) in
  group3d
    (List.init 60 (fun i ->
         let a = at i and b = at (i +.. 1) in
         polygon3d (rgb 55 55 80) [ side a (-0.02); side a 0.02; side b 0.02; side b (-0.02) ]))

(* the black box around it all, its faces turned inwards, and stars on
 * its walls (a bit inside, so they're in front of the walls) *)
let space : shape3d list =
  let w = 150. in
  let wall corners = polygon3d black corners in
  let walls =
    [ wall [ (-.w, -.w, -.w); (w, -.w, -.w); (w, w, -.w); (-.w, w, -.w) ];
      wall [ (-.w, -.w, w); (-.w, w, w); (w, w, w); (w, -.w, w) ];
      wall [ (-.w, -.w, -.w); (-.w, w, -.w); (-.w, w, w); (-.w, -.w, w) ];
      wall [ (w, -.w, -.w); (w, -.w, w); (w, w, w); (w, w, -.w) ];
      wall [ (-.w, -.w, -.w); (-.w, -.w, w); (w, -.w, w); (w, -.w, -.w) ];
      wall [ (-.w, w, -.w); (w, w, -.w); (w, w, w); (-.w, w, w) ] ]
  in
  (* stars: a linear congruential generator, the same every time; each a
   * small square facing the center *)
  let rec stars n seed acc =
    if n = 0 then acc
    else
      let next s = ((s *.. 1103515245) +.. 12345) land 0x7fffffff in
      let s1 = next seed in
      let s2 = next s1 in
      let u = float_of_int (s1 mod 2000) / 1000. - 1. and v = float_of_int (s2 mod 2000) / 1000. - 1. in
      let d = w * 0.98 and e = 0.35 in
      (* one of the 4 side walls, by n *)
      let star =
        match n mod 4 with
        | 0 -> [ (u * d, v * d, -.d); (u * d, (v * d) + e, -.d); ((u * d) + e, (v * d) + e, -.d); ((u * d) + e, v * d, -.d) ]
        | 1 -> [ (u * d, v * d, d); ((u * d) + e, v * d, d); ((u * d) + e, (v * d) + e, d); (u * d, (v * d) + e, d) ]
        | 2 -> [ (-.d, v * d, u * d); (-.d, v * d, (u * d) + e); (-.d, (v * d) + e, (u * d) + e); (-.d, (v * d) + e, u * d) ]
        | _ -> [ (d, v * d, u * d); (d, (v * d) + e, u * d); (d, (v * d) + e, (u * d) + e); (d, v * d, (u * d) + e) ]
      in
      stars (n -.. 1) s2 (polygon3d (rgb 180 180 180) (List.rev star) :: acc)
  in
  walls @ stars 240 7 []

(* built once: they never change *)
let still : shape3d = cached3d (space @ List.map orbit_ribbon Planets.planets)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the date: as in the 2D version, Howard Hinnant's civil_from_days,
 * http://howardhinnant.github.io/date_algorithms.html *)
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

let view (computer : computer) (model : model) : camera * shape3d list =
  let s = model.scene and screen = computer.screen in
  let cam = Camera3d.orbit ~distance:s.distance ~height:(s.distance * 0.45) ~look:0. s.turn (0., 0., 0.) in
  let bodies =
    List.map
      (fun (p : Planets.planet) ->
        let pos = to_scene (Kepler.position p.orbit ~days:s.days) in
        (p, pos))
      Planets.planets
  in
  let planets =
    List.concat_map
      (fun ((p : Planets.planet), ((x, y, z) as pos)) ->
        let r = size p.radius in
        [ planet_shape (color_of p.name) r pos ]
        @ (if p.name = "Saturn" then [ rings r |> move3d x y z ] else [])
        @
        if p.name = "Earth" then
          (* the Moon, 0.6 from the Earth (magnified), in the ecliptic *)
          let a = 2. * Float.pi * s.days / Planets.moon_period in
          let (mx, mz) = (x + (0.6 * cos a), z - (0.6 * sin a)) in
          [ planet_shape (200, 200, 200) (size Planets.moon_radius) (mx, y, mz) ]
        else [])
      bodies
  in
  let labels =
    List.filter_map
      (fun ((p : Planets.planet), pos) ->
        Option.map (fun (x, y) -> hud (text (rgb 180 180 180) 1.3 p.name |> move (x + 10.) (y + 14.))) (project cam screen pos))
      bodies
  in
  ( cam,
    [ still; sphere (rgb 255 210 80) sun_size ]
    @ planets @ labels
    @ [ hud (text white 3. (date s.days) |> move (screen.left + 150.) (screen.top - 40.));
        hud
          (text (rgb 200 200 200) 2. (Printf.sprintf "%g days a second%s" s.speed (if s.paused then " (paused)" else ""))
          |> move (screen.left + 150.) (screen.top - 75.));
        hud
          (text (rgb 150 150 150) 1.4 "arrows: the camera   w/s: faster/slower   space: pause   r: back to 2000"
          |> move 0. (screen.bottom + 25.)) ] )

let help =
  {|SolarSystem3d
  keys:  left/right  turn the camera     up/down  nearer/farther
         w/s         faster/slower       space    pause
         r           back to 2000-01-01
|}

let app = game3d view update initial_model

let main =
  print_string help;
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = No_lighting } app
