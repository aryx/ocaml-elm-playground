(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Stellarium (Fabien Chereau, 2001, and its
 * contributors since), the free planetarium: the sky of a place at an
 * instant, drawn as you would see it standing there -- the stars and
 * their constellations, the Sun, the Moon with its phase, the five
 * planets the eye sees, a ground hiding what has set, a sky that is
 * blue by day -- and time that can run a hundred thousand times too
 * fast, so that the sky turns, the Moon waxes and the planets wander.
 * Stellarium's own ancestors are the planetarium projectors (Zeiss's
 * Mark I, Munich, 1923: a dome, the stars projected from inside), and
 * on home computers SkyGlobe (Mark Haun, 1989) and Voyager (Carina
 * Software, 1988).
 *
 *   arrows, drag   look around          + - (or the wheel)  zoom
 *   j k l          time slower, normal, faster (and backwards)
 *   8              back to now          click   what is that star?
 *   c n e          the constellations, the names, the equator and ecliptic
 *   g a            the ground, the atmosphere
 *   p              the next place       d       the whole dome, and back
 *
 * flags place=paris|greenwich|giza|quito|sydney|pole, date=2026-09-26
 * or date=2026-09-26T21:30 (UTC; -2800-06-21 for the pyramids' sky),
 * view=dome.
 *
 * What it teaches is appkits/astronomy's pipeline (Celestial.mli): a
 * catalogue gives a star's place on the celestial sphere, and what you
 * see is that sphere turned by the sidereal time (the Earth's rotation
 * against the stars, 23h56m a turn) and tilted by your latitude -- the
 * same stars at Paris and Sydney, but not the same half of them, and
 * the pole star as high above the horizon as you are north of the
 * equator (try place=pole and place=quito). The Sun, the Moon and the
 * planets are computed, not catalogued: Kepler's ellipses, seen from
 * one of them (Ephemeris.mli). With time running fast (l, a few
 * times) the ecliptic (e) shows why the planets are where they are:
 * all of them keep near the Sun's path, and Mars stops and backs up
 * (its retrograde loop, the puzzle Ptolemy's epicycles answered) while
 * the Earth overtakes it. And precession: date=-2800-06-21 with
 * place=giza puts Thuban, not Polaris, at the pole.
 *
 * The sky is drawn in the stereographic projection (Stereographic.mli),
 * Stellarium's default: circles stay circles, so the horizon is one,
 * and the ground is the outside of it -- two polygons, each half of a
 * big rectangle less half of the disk.
 *
 * Uses: appkits/astronomy (Celestial, Ephemeris, Bright_stars,
 * Stereographic), core's Civil and Clock (the date shown), Scene2d
 * (keys pressed, not held); not the gui toolkit, not Camera2d (the
 * projection is the camera).
 *
 * Exercises: the whole Yale catalogue (9110 stars, to magnitude 6.5)
 * instead of 150, and the Milky Way; atmospheric refraction (the Sun
 * sets half a degree late) and the Moon's parallax (a degree, from
 * the Earth's surface rather than its centre); the planets'
 * magnitudes from their distances and phases (Mars from -2.9 to 1.8);
 * a landscape panorama instead of a flat ground, as Stellarium has;
 * the time zone of each place instead of UTC; eclipses (the Moon on
 * the Sun, date=1999-08-11T10:30 place=paris).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type place = { pname : string; lat : float; lon : float (* degrees, east positive *) }

(* Stellarium's default place first *)
let places =
  [ { pname = "Paris"; lat = 48.8566; lon = 2.3522 };
    { pname = "Greenwich"; lat = 51.4769; lon = -0.0005 };
    { pname = "Giza"; lat = 29.9792; lon = 31.1342 };
    { pname = "Quito"; lat = -0.1807; lon = -78.4678 };
    { pname = "Sydney"; lat = -33.8688; lon = 151.2093 };
    { pname = "Pole"; lat = 90.; lon = 0. } ]

(* the speeds of time, as Stellarium's j and l step through them *)
let rates = [| -100000.; -10000.; -1000.; -100.; -10.; -1.; 0.; 1.; 10.; 100.; 1000.; 10000.; 100000. |]
let real_rate = 7

type sky = {
  place : int;
  sim : float; (* the instant shown, Unix seconds (UTC) *)
  rate : int; (* an index in [rates] *)
  last : float option; (* the real time of the frame before *)
  view_az : float; (* degrees, 180 = south *)
  view_alt : float;
  fov : float; (* degrees across the screen's smaller side *)
  lines : bool;
  names : bool;
  grid : bool;
  ground : bool;
  atmosphere : bool;
  selected : string option;
  dragged : float; (* how far the mouse moved since pressed: a drag, not a click *)
  started : bool; (* the flags read *)
}

type model = sky Scene2d.t

let initial_model : model =
  Scene2d.start
    { place = 0; sim = 0.; rate = real_rate; last = None; view_az = 180.; view_alt = 25.; fov = 100.; lines = true;
      names = true; grid = false; ground = true; atmosphere = true; selected = None; dragged = 0.; started = false }

let clamp lo hi x = Float.max lo (Float.min hi x)
let seconds (computer : computer) : float = match computer.time with Time t -> t

(*****************************************************************************)
(* The sky at an instant *)
(*****************************************************************************)

(* everything drawn as a point, with what the info line says of it *)
type kind = Star of char | Planet of Ephemeris.planet | Sun | Moon of float (* lit *)

type body = { label : string; detail : string; eq : Celestial.equatorial; hz : Celestial.horizontal; mag : float; kind : kind }

(* the planets' colours and their typical magnitudes (Mars varies the
 * most: see the exercises) *)
let planet_look (p : Ephemeris.planet) : color * float =
  match p with
  | Mercury -> (rgb 200 190 180, 0.)
  | Venus -> (rgb 255 250 225, -4.2)
  | Mars -> (rgb 255 140 90, 0.7)
  | Jupiter -> (rgb 240 225 200, -2.3)
  | Saturn -> (rgb 235 215 160, 0.6)

(* the bodies, the Sun's place (equatorial and horizontal) and the
 * local sidereal time *)
let bodies (s : sky) : body list * (Celestial.equatorial * Celestial.horizontal) * float =
  let p = List.nth places s.place in
  let jd = Celestial.julian_date s.sim in
  let lst = Celestial.lst jd ~lon:(Celestial.degrees p.lon) in
  let hz eq = Celestial.to_horizontal ~lat:(Celestial.degrees p.lat) ~lst eq in
  let stars =
    List.map
      (fun (st : Bright_stars.star) ->
        let eq = Celestial.precess jd st.pos in
        { label = (if st.name = "" then st.id else st.name); detail = (if st.name = "" then "" else st.id); eq;
          hz = hz eq; mag = st.mag; kind = Star st.spectral })
      Bright_stars.stars
  in
  let planets =
    List.map
      (fun pl ->
        let eq = Celestial.precess jd (Ephemeris.planet pl jd) in
        { label = Ephemeris.planet_name pl; detail = "planet"; eq; hz = hz eq; mag = snd (planet_look pl); kind = Planet pl })
      Ephemeris.planets
  in
  let sun = Celestial.precess jd (Ephemeris.sun jd) in
  let moon = Ephemeris.moon jd in
  let lit = Ephemeris.illuminated ~sun moon in
  ( stars @ planets
    @ [ { label = "Moon"; detail = Printf.sprintf "%.0f%% lit" (lit *. 100.); eq = moon; hz = hz moon; mag = -12.7;
          kind = Moon lit };
        { label = "Sun"; detail = "star, class G"; eq = sun; hz = hz sun; mag = -26.7; kind = Sun } ],
    (sun, hz sun),
    lst )

(*****************************************************************************)
(* The projection on the screen *)
(*****************************************************************************)

(* pixels per unit of the projection's plane: [fov] degrees across the
 * screen's smaller side, 2 tan (fov/4) units from the centre to it *)
let zoom (computer : computer) (s : sky) : float =
  Float.min computer.screen.width computer.screen.height /. (4. *. tan (Celestial.degrees s.fov /. 4.))

let view_dir (s : sky) : Celestial.horizontal = { az = Celestial.degrees s.view_az; alt = Celestial.degrees s.view_alt }

let to_screen (k : float) (s : sky) (h : Celestial.horizontal) : (float * float) option =
  Option.map (fun (x, y) -> (k *. x, k *. y)) (Stereographic.project ~view:(view_dir s) h)

let visible (s : sky) (h : Celestial.horizontal) : bool = (not s.ground) || h.alt > 0.

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* "2026-09-26", "2026-09-26T21:30", "-2800-06-21": Unix seconds, UTC *)
let parse_date (str : string) : float option =
  let neg = String.length str > 0 && str.[0] = '-' in
  let str = if neg then String.sub str 1 (String.length str - 1) else str in
  let date, time = match String.split_on_char 'T' str with [ d ] -> (d, "0:0") | [ d; t ] -> (d, t) | _ -> ("", "") in
  match (List.map int_of_string_opt (String.split_on_char '-' date), List.map int_of_string_opt (String.split_on_char ':' time)) with
  | [ Some y; Some m; Some d ], [ Some hh; Some mm ] ->
      let date : Civil.date = { year = (if neg then -y else y); month = m; day = d } in
      if Civil.is_valid date then
        Some (Clock.of_local ~offset:0 date { hour = hh; minute = mm; second = 0. })
      else None
  | _ -> None

let from_flags (flags : flags) (real : float) (s : sky) : sky =
  let s = { s with sim = Option.value ~default:real (Option.bind (List.assoc_opt "date" flags) parse_date) } in
  let s =
    match List.assoc_opt "place" flags with
    | Some name -> (
        let rec find i = function
          | [] -> None
          | p :: ps -> if String.lowercase_ascii p.pname = String.lowercase_ascii name then Some i else find (i + 1) ps
        in
        match find 0 places with Some i -> { s with place = i } | None -> s)
    | None -> s
  in
  let s = if List.assoc_opt "view" flags = Some "dome" then { s with view_alt = 90.; fov = 180. } else s in
  { s with started = true }

(* the body nearest the mouse, within a few pixels *)
let pick (computer : computer) (s : sky) : string option =
  let k = zoom computer s in
  let all, _, _ = bodies s in
  List.fold_left
    (fun best b ->
      match to_screen k s b.hz with
      | Some (x, y) when visible s b.hz ->
          let d = Float.hypot (x -. computer.mouse.mx) (y -. computer.mouse.my) in
          (match best with Some (_, d') when d' <= d -> best | _ -> if d < 12. then Some (b.label, d) else best)
      | _ -> best)
    None all
  |> Option.map fst

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let key name = Scene2d.pressed (fun k -> Set_.mem name k.keys) m in
  let held name = Set_.mem name computer.keyboard.keys in
  let real = seconds computer in
  let s = if m.scene.started then m.scene else from_flags computer.flags real m.scene in
  (* time runs at its rate *)
  let dt = match s.last with Some t -> real -. t | None -> 0. in
  let s = { s with sim = s.sim +. (dt *. rates.(s.rate)); last = Some real } in
  let s =
    if key "j" then { s with rate = max 0 (s.rate - 1) }
    else if key "k" then { s with rate = real_rate }
    else if key "l" then { s with rate = min (Array.length rates - 1) (s.rate + 1) }
    else if key "8" then { s with sim = real; rate = real_rate }
    else s
  in
  (* the toggles *)
  let s = if key "c" then { s with lines = not s.lines } else s in
  let s = if key "n" then { s with names = not s.names } else s in
  let s = if key "e" then { s with grid = not s.grid } else s in
  let s = if key "g" then { s with ground = not s.ground } else s in
  let s = if key "a" then { s with atmosphere = not s.atmosphere } else s in
  let s = if key "p" then { s with place = (s.place + 1) mod List.length places } else s in
  let s =
    if key "d" then if s.view_alt > 89. && s.fov > 170. then { s with view_alt = 25.; fov = 100. } else { s with view_alt = 90.; fov = 180. }
    else s
  in
  (* looking around: the arrows, and the sky dragged along with the
   * mouse (a pixel is 1/k radian near the centre) *)
  let k = zoom computer s in
  let step = s.fov /. 100. in
  let mouse = computer.mouse in
  let drag_az, drag_alt = if mouse.mdown then (Celestial.to_degrees (mouse.mdx /. k), Celestial.to_degrees (mouse.mdy /. k)) else (0., 0.) in
  let az = s.view_az +. (to_x computer.keyboard *. step) -. drag_az in
  let alt = s.view_alt +. (to_y computer.keyboard *. step) -. drag_alt in
  (* with the ground on, the view stays above the horizon, where the
   * horizon is a circle (Stereographic.horizon) *)
  let alt = if s.ground then clamp 2. 90. alt else clamp (-90.) 90. alt in
  let fov = s.fov *. (0.9 ** mouse.mwheel) in
  let fov = if held "+" || held "=" then fov *. 0.97 else if held "-" then fov /. 0.97 else fov in
  let s = { s with view_az = Float.rem (az +. 360.) 360.; view_alt = alt; fov = clamp 5. 180. fov } in
  (* a click, not the end of a drag, picks what is under the mouse *)
  let s = { s with dragged = (if mouse.mdown then s.dragged +. Float.abs mouse.mdx +. Float.abs mouse.mdy else s.dragged) } in
  let s = if mouse.mclick then { s with selected = (if s.dragged < 4. then pick computer s else s.selected); dragged = 0. } else s in
  { m with scene = s }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (c : color) (size : number) (str : string) : shape = words c str |> scale size

let segment (c : color) (width : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle c (Float.hypot dx dy) width |> rotate (Celestial.to_degrees (atan2 dy dx)) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let mix (a : int * int * int) (b : int * int * int) (t : float) : color =
  let t = clamp 0. 1. t in
  let m x y = int_of_float ((float_of_int x *. (1. -. t)) +. (float_of_int y *. t)) in
  let r1, g1, b1 = a and r2, g2, b2 = b in
  rgb (m r1 r2) (m g1 g2) (m b1 b2)

(* the daylight, 0 at night (the Sun 12 deg below the horizon, the end
 * of nautical twilight) to 1 (6 deg above it) *)
let daylight (s : sky) (sun : Celestial.horizontal) : float =
  if s.atmosphere then clamp 0. 1. ((Celestial.to_degrees sun.alt +. 12.) /. 18.) else 0.

(* the faintest magnitude seen: 6.5 under a dark sky, -3 in daylight
 * (Venus, sometimes) *)
let limit (s : sky) (sun : Celestial.horizontal) : float =
  if not s.atmosphere then 6.5 else clamp (-3.) 6.5 (-3. -. (Celestial.to_degrees sun.alt *. 9.5 /. 18.))

(* a polyline on the sky, the pieces on the screen and above the
 * ground *)
let curve (k : float) (s : sky) (c : color) (points : Celestial.horizontal list) : shape list =
  let rec go = function
    | a :: (b :: _ as rest) -> (
        match (to_screen k s a, to_screen k s b) with
        | Some pa, Some pb when visible s a && visible s b -> segment c 1.5 pa pb :: go rest
        | _ -> go rest)
    | _ -> []
  in
  go points

(* the Moon's disk lit on the side of [angle] (degrees): the limb a
 * half-circle, the terminator a half-ellipse, (1 - 2 lit) wide *)
let moon_disk (r : number) (lit : float) (angle : number) : shape =
  let half a0 a1 f = List.init 19 (fun i -> let t = Celestial.degrees (a0 +. ((a1 -. a0) *. float_of_int i /. 18.)) in f t) in
  let limb = half (-90.) 90. (fun t -> (r *. cos t, r *. sin t)) in
  let terminator = half 90. (-90.) (fun t -> ((1. -. (2. *. lit)) *. r *. cos t, r *. sin t)) in
  group [ circle (rgb 50 50 58) r; polygon (rgb 240 238 225) (limb @ terminator) |> rotate angle ]

let draw_body (k : float) (s : sky) ~(lim : float) ~(sun : Celestial.equatorial) ~(lst : float) (b : body) (x, y) : shape list =
  let size = clamp 0.7 2. (sqrt (100. /. s.fov)) in
  let alpha = clamp 0. 1. (lim -. b.mag) in
  let r = clamp 1. 6. (1. +. (0.9 *. (4.5 -. b.mag))) *. size in
  let point c = if alpha <= 0. then [] else [ circle c (r *. 2.4) |> fade (0.15 *. alpha); circle c r |> fade alpha ] in
  (* the Sun and the Moon are half a degree wide, drawn at least a few
   * pixels *)
  let disk = Float.max 9. (k *. Celestial.degrees 0.5 /. 2.) in
  List.map (move x y)
    (match b.kind with
    | Star cls -> let r, g, bl = Bright_stars.color cls in point (rgb r g bl)
    | Planet pl -> point (fst (planet_look pl))
    | Sun -> [ circle (rgb 255 240 180) (disk *. 2.5) |> fade 0.25; circle (rgb 255 250 220) disk ]
    | Moon lit ->
        (* lit towards the Sun: a point a little way along the great
         * circle to it, projected, gives the direction on the screen *)
        let angle =
          let mx, my, mz = Celestial.to_vector b.eq and sx, sy, sz = Celestial.to_vector sun in
          let toward = Celestial.of_vector (mx +. (0.02 *. (sx -. mx)), my +. (0.02 *. (sy -. my)), mz +. (0.02 *. (sz -. mz))) in
          let p = List.nth places s.place in
          match to_screen k s (Celestial.to_horizontal ~lat:(Celestial.degrees p.lat) ~lst toward) with
          | Some (tx, ty) -> Celestial.to_degrees (atan2 (ty -. y) (tx -. x))
          | None -> 0.
        in
        [ moon_disk disk lit angle ])

(* the ground: outside the horizon's circle, as two polygons, the left
 * and the right halves of a rectangle bigger than the circle, each less
 * its half of the disk *)
let ground (k : float) (s : sky) (c : color) : shape list =
  let cy, r = Stereographic.horizon (Celestial.degrees s.view_alt) in
  let cy = k *. cy and r = k *. r in
  let big = 3000. +. Float.abs cy +. r in
  let arc = List.init 91 (fun i -> let t = Celestial.degrees (270. -. (float_of_int i *. 2.)) in (r *. cos t, cy +. (r *. sin t))) in
  let left = [ (0., big); (-.big, big); (-.big, -.big); (0., -.big) ] @ arc in
  [ polygon c left; polygon c (List.map (fun (x, y) -> (-.x, y)) left) ]

let view (computer : computer) (m : model) : shape list =
  let s = m.scene in
  let k = zoom computer s in
  let all, (sun_eq, sun), lst = bodies s in
  let light = daylight s sun in
  let lim = limit s sun in
  let w = computer.screen.width and h = computer.screen.height in
  let on_screen (x, y) = Float.abs x < (w /. 2.) +. 50. && Float.abs y < (h /. 2.) +. 50. in
  let placed = List.filter_map (fun b -> Option.map (fun xy -> (b, xy)) (to_screen k s b.hz)) all in
  let placed = List.filter (fun (_, xy) -> on_screen xy) placed in
  let find label = List.find_opt (fun (b, _) -> b.label = label || b.detail = label) placed in
  let star_xy id = Option.bind (Bright_stars.find id) (fun (st : Bright_stars.star) -> find (if st.name = "" then st.id else st.name)) in
  let p = List.nth places s.place in
  let jd = Celestial.julian_date s.sim in
  let hz eq = Celestial.to_horizontal ~lat:(Celestial.degrees p.lat) ~lst eq in
  (* the equator and the ecliptic, of the date *)
  let grid =
    if not s.grid then []
    else
      let around f = List.init 73 (fun i -> f (Celestial.degrees (float_of_int i *. 5.))) in
      curve k s (rgb 150 60 60) (around (fun ra -> hz { ra; dec = 0. }))
      @ curve k s (rgb 150 130 50) (around (fun lon -> hz (Celestial.of_ecliptic ~obliquity:(Celestial.obliquity jd) lon 0.)))
  in
  let figures =
    if not s.lines then []
    else
      List.concat_map
        (fun (f : Bright_stars.figure) ->
          let lines =
            List.filter_map
              (fun (a, b) ->
                match (star_xy a, star_xy b) with
                | Some (ba, pa), Some (bb, pb) when visible s ba.hz && visible s bb.hz ->
                    (* drawn by the program, not seen: fainter by day *)
                    Some (segment (rgb 50 80 130) 1.5 pa pb |> fade (1. -. (0.6 *. light)), pa)
                | _ -> None)
              f.lines
          in
          (* the name at the middle of what is drawn of the figure *)
          let name =
            match lines with
            | [] -> []
            | _ when s.names ->
                let n = float_of_int (List.length lines) in
                let x = List.fold_left (fun a (_, (x, _)) -> a +. x) 0. lines /. n and y = List.fold_left (fun a (_, (_, y)) -> a +. y) 0. lines /. n in
                [ text (rgb 70 110 170) 1.2 (String.uppercase_ascii f.constellation) |> move x (y -. 20.) |> fade (1. -. (0.6 *. light)) ]
            | _ -> []
          in
          List.map fst lines @ name)
        Bright_stars.figures
  in
  let points = List.concat_map (fun (b, xy) -> draw_body k s ~lim ~sun:sun_eq ~lst b xy) placed in
  let cardinals =
    List.filter_map
      (fun (label, az) ->
        Option.map (fun (x, y) -> text (rgb 230 140 60) 2. label |> move x (y -. 18.))
          (to_screen k s { az = Celestial.degrees az; alt = 0. }))
      [ ("N", 0.); ("E", 90.); ("S", 180.); ("W", 270.) ]
  in
  let labels =
    if not s.names then []
    else
      List.filter_map
        (fun (b, (x, y)) ->
          let named = match b.kind with Star _ -> b.detail <> "" && b.mag < 2.6 && b.mag < lim | _ -> b.mag < lim || b.kind = Sun in
          if named && visible s b.hz then Some (text (rgb 190 200 220) 1.2 b.label |> move (x +. 12.) (y +. 12.)) else None)
        placed
  in
  let selected =
    match Option.bind s.selected find with
    | Some (_, (x, y)) ->
        (* Stellarium's four ticks around the selection *)
        [ segment (rgb 255 200 80) 2. (x -. 16., y) (x -. 8., y); segment (rgb 255 200 80) 2. (x +. 8., y) (x +. 16., y);
          segment (rgb 255 200 80) 2. (x, y -. 16.) (x, y -. 8.); segment (rgb 255 200 80) 2. (x, y +. 8.) (x, y +. 16.) ]
    | None -> []
  in
  let info =
    match Option.bind s.selected (fun l -> List.find_opt (fun b -> b.label = l) all) with
    | Some b ->
        let deg = Celestial.to_degrees in
        let kind = match b.kind with Star cls -> Printf.sprintf "%s, class %c" b.detail cls | _ -> b.detail in
        Printf.sprintf "%s (%s)  mag %.1f  RA %.2fh Dec %+.1f  alt %.1f az %.1f" b.label kind b.mag (Celestial.to_hours b.eq.ra)
          (deg b.eq.dec) (deg b.hz.alt) (deg b.hz.az)
    | None -> "click a star or a planet"
  in
  let date, tod = Clock.local ~offset:0 s.sim in
  let rate = rates.(s.rate) in
  let lst_h = Celestial.to_hours lst in
  let bar y str = [ rectangle (rgb 0 0 0) w 30. |> fade 0.6 |> move_y y; text (rgb 200 200 200) 1.4 str |> move_y y ] in
  [ rectangle (mix (4, 6, 18) (70, 130, 200) light) w h ]
  @ grid @ figures @ points
  @ (if s.ground then ground k s (mix (12, 18, 12) (60, 80, 50) light) else [])
  @ cardinals @ labels @ selected
  @ bar ((h /. 2.) -. 15.)
      (Printf.sprintf "TINY STELLARIUM   %s %.2f%c %.2f%c   %s %s UTC   time x%s   sidereal %02d:%02d" p.pname (Float.abs p.lat)
         (if p.lat >= 0. then 'N' else 'S') (Float.abs p.lon) (if p.lon >= 0. then 'E' else 'W') (Civil.to_string date)
         (Clock.to_string ~seconds:false tod) (if Float.is_integer rate then Printf.sprintf "%.0f" rate else string_of_float rate)
         (int_of_float lst_h) (int_of_float (Float.rem (lst_h *. 60.) 60.)))
  @ bar ((-.h /. 2.) +. 45.) info
  @ bar ((-.h /. 2.) +. 15.) "arrows/drag look  +- zoom  j k l time  8 now  c lines  n names  e grid  g ground  a air  p place  d dome"

let app = game view update initial_model

let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
