(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Elite (David Braben and Ian Bell, Acornsoft, 1984),
 * in the *2D* playground: a Cobra in orbit round Lave, a few pirates to
 * fight off, and the Coriolis station to dock with -- which is the
 * hard part, as it always was. Up/down to pitch, left/right to roll,
 * w/s for more or less speed, space to fire the laser.
 *
 * Elite ran on a BBC Micro: a 2 MHz 6502, no hardware multiply, no 3D
 * of any kind, 32 KB for everything. Inside it was a real, if small, 3D
 * engine, and that engine is the trick of this game, written out here
 * the way it was written then:
 *
 *  - You never move. The player's ship sits at the origin, looking
 *    down +z, for ever; flying is the *universe* turning round you.
 *    Every frame, every other object's position and orientation are
 *    rotated by your pitch and roll, the other way, and slid back by
 *    your speed. So there is no camera to transform anything by: the
 *    world is already in the eye's coordinates ([turn_world]).
 *
 *  - A ship's orientation is three vectors, nose, roof and side, which
 *    is a rotation matrix written a row at a time (gamekits/segments/
 *    Sixdof.mli says the same, for TinyDescent). Sixdof turns
 *    them with the real cosine and sine and straightens them after
 *    every turn. The 6502 had neither, so Elite turned them by
 *
 *        sin a ~ a        cos a ~ 1 - a^2/2
 *
 *    which is wrong by a hair every frame, and every few frames it
 *    straightened them back to square ([small_turn], [tidy]). The same
 *    idea as Sixdof's, bought at 1984 prices.
 *
 *  - A ship is a convex hull: its corners, its faces, and each edge
 *    between two of them. A face is visible when it faces you (its
 *    normal against the line of sight, one dot product); an edge is
 *    drawn when *either* of its two faces is ([visible_edges]). That is
 *    hidden-line removal, and it is why Elite's wireframes read as
 *    solid ships where TinyBattlezone's, which draw every edge,
 *    are see-through:
 *
 *            _______            _______
 *           /|     /|          /      /|    the same box: every edge
 *          /_|____/ |         /______/ |    (Battlezone), and only the
 *          | |____|_|         |      | |    edges of the faces turned
 *          | /    | /         |      | /    towards you (Elite)
 *          |/_____|/          |______|/
 *
 *    It only works because every ship is convex -- a hull cannot hide
 *    part of itself behind another part -- and it never hides one ship
 *    behind another: two ships that overlap on screen show through
 *    each other, in Elite as here.
 *
 *  - Perspective is a divide by the depth ([project]), and a line
 *    going behind you is cut where it crosses a plane just ahead of
 *    the eye first ([clip_near]).
 *
 * The planet and the sun were not polygons in Elite either: a circle,
 * with a line across it. Here too.
 *
 * The other thing Elite had in 32 KB was a *galaxy*: eight of them, 256
 * systems each, every one with a name, a government, an economy and a
 * tech level -- stored as six bytes. The systems are not stored at all;
 * they are generated, in order, from a seed of three 16-bit numbers
 * twisted a little per system ([twist], [system]), and the same seed
 * always gives the same galaxy. It starts with Tibedied, and Lave, the
 * system every game began in, is the eighth: a Dictatorship, Rich
 * Agricultural, Tech Level 5, as the manual always said.
 *
 * Why 2.5D: README-2.5d.md calls these worlds "a flat map
 * with one height per point", and Elite's is not. It is one for the
 * reason TinyDescent is: it is full 3D with no 3D engine but
 * its own, all of it written in the file, on the 2D playground. Descent
 * gets the depths right by drawing cells in order; Elite never orders
 * anything, and draws only lines.
 *
 * Uses: the 2D playground and Scene2d, and nothing else -- the engine is
 * the game. Not gamekits/segments/Sixdof (its turns are exact; these are
 * the 6502's), not Playground3d (it would do the projection and the
 * hidden surfaces, which are the lesson).
 *
 * References: the BBC Micro Elite source, annotated by Mark Moxon
 * (bbcelite.com): MVS4 and MVS5 for the small-angle rotations, TIDY
 * for the straightening, LL9 for the hidden lines, PROJ for the
 * projection; Ian Bell's Text Elite (txtelite.c) for the galaxy.
 *
 * Exercises: trading (the market prices come out of the same seed and
 * the system's economy); a hyperspace jump to the next system, which is
 * one more twist of the seed; the docking computer ("Blue Danube"); a
 * sun, and heat, when you fly too near it; the missile, which homes by
 * turning its nose exactly as the pirates do here.
 *)
open Playground

(*****************************************************************************)
(* Vectors *)
(*****************************************************************************)

type vec = { x : number; y : number; z : number }

let v (x : number) (y : number) (z : number) : vec = { x; y; z }
let add (a : vec) (b : vec) : vec = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }
let sub (a : vec) (b : vec) : vec = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }
let times (k : number) (a : vec) : vec = { x = k *. a.x; y = k *. a.y; z = k *. a.z }
let dot (a : vec) (b : vec) : number = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

let cross (a : vec) (b : vec) : vec =
  { x = (a.y *. b.z) -. (a.z *. b.y); y = (a.z *. b.x) -. (a.x *. b.z); z = (a.x *. b.y) -. (a.y *. b.x) }

let length (a : vec) : number = Float.sqrt (dot a a)
let normalize (a : vec) : vec = let l = length a in if l = 0. then a else times (1. /. l) a

(*****************************************************************************)
(* Orientation, at 1984 prices -- the trick of this game, first part *)
(*****************************************************************************)

(* nose, roof and side: where the ship points, where its canopy points,
 * and its right. Three rows of a rotation matrix, carried with it. *)
type orientation = { nose : vec; roof : vec; side : vec }

let upright : orientation = { nose = v 0. 0. 1.; roof = v 0. 1. 0.; side = v 1. 0. 0. }

(* Turning [a] towards [b] by a small angle [t] (radians), the 6502's
 * way: a' = a (1 - t^2/2) + b t, and b' = b (1 - t^2/2) - a t. No cosine
 * and no sine; wrong by about t^3/6, which is nothing for one frame and
 * something after a thousand. *)
let small_turn (t : number) (a : vec) (b : vec) : vec * vec =
  let c = 1. -. (t *. t /. 2.) in
  (add (times c a) (times t b), sub (times c b) (times t a))

(* Straightening the three back to square, Elite's TIDY: the nose made
 * a unit vector, the roof made square to it (the part of it along the
 * nose taken out) and a unit vector, and the side made from the two.
 * Gram and Schmidt, 1907 and 1883. *)
let tidy (o : orientation) : orientation =
  let nose = normalize o.nose in
  let roof = normalize (sub o.roof (times (dot o.roof nose) nose)) in
  { nose; roof; side = cross roof nose }

(* A ship turning itself: pitch turns the nose towards the roof, roll
 * turns the side towards the roof. *)
let pitch_by (t : number) (o : orientation) : orientation =
  let nose, roof = small_turn t o.nose o.roof in
  { o with nose; roof }

let roll_by (t : number) (o : orientation) : orientation =
  let side, roof = small_turn t o.side o.roof in
  { o with side; roof }

(* the same small turn, on two coordinates rather than two vectors *)
let small_turn2 (t : number) (a : number) (b : number) : number * number =
  let c = 1. -. (t *. t /. 2.) in
  ((c *. a) +. (t *. b), (c *. b) -. (t *. a))

(* The universe turning round you: a point (or a vector) in your frame,
 * turned by your pitch and roll *the other way* -- nose up, and what
 * is ahead goes down; roll right, and what is right goes up. *)
let turn_world (pitch : number) (roll : number) (p : vec) : vec =
  let x, y = small_turn2 roll p.x p.y in
  let y, z = small_turn2 pitch y p.z in
  { x; y; z }

(*****************************************************************************)
(* Hulls, and the hidden lines -- the trick of this game, second part *)
(*****************************************************************************)

(* A hull: its corners, and its faces as corners in order round each
 * one. The edges and each face's outward normal are worked out from
 * those, once ([hull]): an edge is where two faces meet, and a hull's
 * normal points away from its middle, which is the origin. *)
type hull = {
  corners : vec array;
  normals : vec array; (* one per face, outward *)
  reach : number array; (* per face: how far its plane is from the hull's middle *)
  edges : (int * int * int * int) list; (* two corners, and the two faces they part *)
}

let hull (corners : vec list) (faces : int list list) : hull =
  let corners = Array.of_list corners in
  let normals =
    Array.of_list
      (List.map
         (fun face ->
           let a = corners.(List.nth face 0) and b = corners.(List.nth face 1) and c = corners.(List.nth face 2) in
           let n = normalize (cross (sub b a) (sub c a)) in
           (* outward: away from the middle, which is the origin *)
           if dot n a < 0. then times (-1.) n else n)
         faces)
  in
  let table = Hashtbl.create 32 in
  List.iteri
    (fun f face ->
      let arr = Array.of_list face in
      let n = Array.length arr in
      Array.iteri
        (fun k a ->
          let b = arr.((k + 1) mod n) in
          let key = (min a b, max a b) in
          Hashtbl.replace table key (f :: (try Hashtbl.find table key with Not_found -> [])))
        arr)
    faces;
  let edges =
    Hashtbl.fold
      (fun (a, b) fs acc -> match fs with [ f1; f2 ] -> (a, b, f1, f2) :: acc | _ -> acc)
      table []
  in
  let reach = Array.of_list (List.mapi (fun f face -> dot normals.(f) corners.(List.hd face)) faces) in
  { corners; normals; reach; edges }

(* where a corner of a ship is, in your frame: its own coordinates
 * along the ship's side, roof and nose, from where the ship is *)
let place (pos : vec) (o : orientation) (c : vec) : vec =
  add pos (add (times c.x o.side) (add (times c.y o.roof) (times c.z o.nose)))

let turn_vec (o : orientation) (n : vec) : vec = add (times n.x o.side) (add (times n.y o.roof) (times n.z o.nose))

(* Hidden-line removal, Elite's LL9: a face is visible when it is
 * turned towards you, its normal against the line from you to it;
 * and an edge is drawn when either of its faces is. Nothing else is
 * needed on a convex hull. *)
let visible_edges (h : hull) (pos : vec) (o : orientation) : (vec * vec) list =
  (* The eye, at the origin, is in front of a face's plane when it is
   * further out along the normal than the plane is: -n.pos > reach.
   * Test against a point near the ship's middle instead, as a first
   * version of this did, and the faces nearly edge-on to you -- the
   * ones that decide the outline -- come out wrong. *)
  let facing = Array.mapi (fun f n -> dot (turn_vec o n) pos +. h.reach.(f) < 0.) h.normals in
  List.filter_map
    (fun (a, b, f1, f2) ->
      if facing.(f1) || facing.(f2) then Some (place pos o h.corners.(a), place pos o h.corners.(b)) else None)
    h.edges

(*****************************************************************************)
(* Seeing: the divide by the depth -- the trick of this game, third part *)
(*****************************************************************************)

let near = 2.
let focal = 720.

(* the part of a line in front of the plane z = near: a line wholly
 * behind you is gone, one crossing the plane is cut there, since a
 * point behind you would divide by a negative depth and land on the
 * wrong side of the screen *)
let clip_near ((a, b) : vec * vec) : (vec * vec) option =
  match (a.z >= near, b.z >= near) with
  | true, true -> Some (a, b)
  | false, false -> None
  | _ ->
      let t = (near -. a.z) /. (b.z -. a.z) in
      let cut = add a (times t (sub b a)) in
      if a.z >= near then Some (a, cut) else Some (cut, b)

let project (p : vec) : number * number = (focal *. p.x /. p.z, focal *. p.y /. p.z)

let line (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.max 1. (Float.hypot dx dy)) 2.
  |> rotate (atan2 dy dx *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let draw_hull (color : color) (h : hull) (pos : vec) (o : orientation) : shape list =
  List.filter_map
    (fun seg -> Option.map (fun (a, b) -> line color (project a) (project b)) (clip_near seg))
    (visible_edges h pos o)

(*****************************************************************************)
(* The ships *)
(*****************************************************************************)

(* The Coriolis station: a cuboctahedron, the twelve middles of a cube's
 * edges, which is what the real one is -- six squares and eight
 * triangles. The docking slot is in the square its nose points out of. *)
let coriolis_size = 160.

let coriolis : hull =
  let s = coriolis_size in
  let corners =
    [ v s s 0.; v s (-.s) 0.; v (-.s) s 0.; v (-.s) (-.s) 0.;
      v s 0. s; v s 0. (-.s); v (-.s) 0. s; v (-.s) 0. (-.s);
      v 0. s s; v 0. s (-.s); v 0. (-.s) s; v 0. (-.s) (-.s) ]
  in
  let faces =
    [ (* the six squares *)
      [ 0; 4; 1; 5 ]; [ 2; 7; 3; 6 ]; [ 0; 9; 2; 8 ]; [ 1; 10; 3; 11 ]; [ 4; 8; 6; 10 ]; [ 5; 11; 7; 9 ];
      (* the eight triangles *)
      [ 0; 8; 4 ]; [ 0; 5; 9 ]; [ 1; 4; 10 ]; [ 1; 11; 5 ];
      [ 2; 6; 8 ]; [ 2; 9; 7 ]; [ 3; 10; 6 ]; [ 3; 7; 11 ] ]
  in
  hull corners faces

(* a Sidewinder, the pirates' little fighter: a flat arrowhead, a
 * pyramid on a diamond *)
let sidewinder : hull =
  hull
    [ v 0. 0. 34.; v (-36.) 0. (-18.); v 0. 10. (-18.); v 36. 0. (-18.); v 0. (-10.) (-18.) ]
    [ [ 0; 1; 2 ]; [ 0; 2; 3 ]; [ 0; 3; 4 ]; [ 0; 4; 1 ]; [ 1; 4; 3; 2 ] ]

(*****************************************************************************)
(* The galaxy, from six bytes -- the other thing in 32 KB *)
(*****************************************************************************)

type system = { name : string; government : string; economy : string; tech : int }

(* the next seed: the three 16-bit numbers shifted along, the new one
 * their sum, a Fibonacci sequence with the carries thrown away *)
let twist ((w0, w1, w2) : int * int * int) : int * int * int = (w1, w2, (w0 + w1 + w2) land 0xFFFF)

let letters = "..LEXEGEZACEBISOUSESARMAINDIREA.ERATENBERALAVETIEDORQUANTEISRION"

let governments =
  [| "Anarchy"; "Feudal"; "Multi-Government"; "Dictatorship"; "Communist"; "Confederacy"; "Democracy";
     "Corporate State" |]

let economies =
  [| "Rich Industrial"; "Average Industrial"; "Poor Industrial"; "Mainly Industrial"; "Mainly Agricultural";
     "Rich Agricultural"; "Average Agricultural"; "Poor Agricultural" |]

(* one system from the seed, and the seed after it: a few of its bits
 * are the government and the economy, and its high byte, twisted four
 * times, picks the letter pairs of the name (three pairs, or four when
 * one bit says so; a '.' is no letter at all) *)
let system ((w0, w1, _) as seed : int * int * int) : system * (int * int * int) =
  let government = (w1 lsr 3) land 7 in
  let economy = (w0 lsr 8) land 7 in
  let economy = if government <= 1 then economy lor 2 else economy in
  let tech = ((w1 lsr 8) land 3) + (economy lxor 7) + (government lsr 1) + (government land 1) in
  let long = w0 land 64 <> 0 in
  let rec pairs n s acc = if n = 0 then (List.rev acc, s) else let _, _, w2 = s in pairs (n - 1) (twist s) ((2 * ((w2 lsr 8) land 31)) :: acc) in
  let chosen, seed = pairs 4 seed [] in
  let chosen = if long then chosen else List.filteri (fun i _ -> i < 3) chosen in
  let name =
    String.concat "" (List.map (fun p -> String.sub letters p 2) chosen)
    |> String.split_on_char '.' |> String.concat "" |> String.lowercase_ascii |> String.capitalize_ascii
  in
  ({ name; government = governments.(government); economy = economies.(economy); tech = tech + 1 }, seed)

(* the first galaxy's seed, and its systems in order *)
let first_galaxy = (0x5A4A, 0x0248, 0xB753)

let systems (n : int) : system list =
  let rec go k s acc = if k = 0 then List.rev acc else let sys, s = system s in go (k - 1) s (sys :: acc) in
  go n first_galaxy []

let lave : system = List.nth (systems 8) 7

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* anything out there: where it is and which way it points, in *your*
 * frame, since that is the only frame there is *)
type body = { pos : vec; o : orientation; speed : number }

type pirate = { ship : body; alive : bool; cooldown : int }

type flight = {
  planet : vec; (* the planet's middle: a body too, carried round like the rest *)
  station : body;
  pirates : pirate list;
  speed : number;
  shield : int;
  energy : int;
  laser : int; (* frames the beam shows *)
  kills : int;
  frames : int;
  hit : int;
}

type scene = Title | Flying of flight | Docked of flight | Destroyed of flight
type model = scene Scene2d.t

(* the station ahead, turned a little, its slot facing back at you; a
 * few pirates between *)
let new_flight () : flight =
  let facing_you = { nose = v 0. 0. (-1.); roof = v 0. 1. 0.; side = v (-1.) 0. 0. } in
  let pirate x y z = { ship = { pos = v x y z; o = facing_you; speed = 3.2 }; alive = true; cooldown = 90 } in
  (* the pirates far enough out that you see them coming before they
   * can shoot: a first version started them at 1200 and the shield was
   * a third gone before the player had touched a key *)
  { planet = v (-4000.) 2200. 40000.;
    station = { pos = v 120. (-60.) 3400.; o = tidy (roll_by 0.3 facing_you); speed = 0. };
    pirates = [ pirate (-420.) 80. 2200.; pirate 380. (-40.) 2600.; pirate 40. 220. 3000. ];
    speed = 6.;
    shield = 100;
    energy = 100;
    laser = 0;
    kills = 0;
    frames = 0;
    hit = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let pitch_rate = 0.022
let roll_rate = 0.035

(* a body carried round by your turn and slid back by your speed:
 * position and orientation both, since they are all in your frame *)
let carry (pitch : number) (roll : number) (speed : number) (b : body) : body =
  let pos = turn_world pitch roll b.pos in
  let o = { nose = turn_world pitch roll b.o.nose; roof = turn_world pitch roll b.o.roof; side = turn_world pitch roll b.o.side } in
  { b with pos = { pos with z = pos.z -. speed }; o }

(* A pirate turns its nose towards you by the same small turns, then
 * flies along it: the homing is nothing more than its orientation
 * nudged towards the line to the origin, and straightened. *)
let steer (p : pirate) : pirate =
  let b = p.ship in
  let towards = normalize (times (-1.) b.pos) in
  let nose = normalize (add b.o.nose (times 0.03 towards)) in
  let o = tidy { b.o with nose } in
  { p with ship = { b with o; pos = add b.pos (times b.speed o.nose) } }

(* the station turns slowly about its own nose, as the real one did: to
 * dock, you match its roll *)
let spin (b : body) : body = { b with o = roll_by 0.012 b.o }

(* You are docking when you are at the slot -- the middle of the square
 * the station's nose points out of -- slowly, pointing into it, and
 * rolled to match it: the slot is a letterbox, and it turns. *)
let docking (f : flight) : bool =
  let slot = add f.station.pos (times coriolis_size f.station.o.nose) in
  let into = dot (v 0. 0. 1.) (times (-1.) f.station.o.nose) in
  (* the slot's roof, seen from you: square to your view, it must be
   * near your own up *)
  let roof = f.station.o.roof in
  let tilt = Float.abs (atan2 roof.x roof.y) in
  length slot < 70. && into > 0.9 && tilt < 0.4 && f.speed < 8.

let crashed (f : flight) : bool = length f.station.pos < coriolis_size *. 1.05 && not (docking f)

let axis (a : bool) (b : bool) : number = (if a then 1. else 0.) -. if b then 1. else 0.

let step_flight (keys : keyboard) (fire : bool) (f : flight) : flight =
  let pitch = axis keys.kup keys.kdown *. pitch_rate and roll = axis keys.kleft keys.kright *. roll_rate in
  let speed = Basics.clamp 0. 14. (f.speed +. (axis (Set_.mem "w" keys.keys) (Set_.mem "s" keys.keys) *. 0.15)) in
  let planet = let p = turn_world pitch roll f.planet in { p with z = p.z -. speed } in
  let station = spin (carry pitch roll speed f.station) in
  let pirates = List.map (fun p -> if p.alive then steer { p with ship = carry pitch roll speed p.ship } else p) f.pirates in
  (* every sixteen frames, everything straightened, as Elite did: not
   * every frame, since TIDY cost a 6502 dearly *)
  let straighten (b : body) = if f.frames mod 16 = 0 then { b with o = tidy b.o } else b in
  let station = straighten station in
  let pirates = List.map (fun p -> { p with ship = straighten p.ship }) pirates in
  (* the laser: straight ahead, and it hits whatever sits on the
   * crosshair, near enough *)
  let firing = fire && f.energy > 4 in
  let pirates, killed =
    List.fold_right
      (fun (p : pirate) (acc, n) ->
        let on_sight = p.ship.pos.z > 0. && Float.hypot p.ship.pos.x p.ship.pos.y < 40. +. (p.ship.pos.z *. 0.02) in
        if firing && p.alive && on_sight && p.ship.pos.z < 2400. then ({ p with alive = false } :: acc, n + 1)
        else (p :: acc, n))
      pirates ([], 0)
  in
  (* a pirate with its nose on you, near enough, fires back *)
  let shots = ref 0 in
  let pirates =
    List.map
      (fun (p : pirate) ->
        if not p.alive then p
        else
          let aimed = dot p.ship.o.nose (normalize (times (-1.) p.ship.pos)) > 0.97 in
          if aimed && p.cooldown = 0 && length p.ship.pos < 700. then begin
            incr shots;
            { p with cooldown = 110 }
          end
          else { p with cooldown = max 0 (p.cooldown - 1) })
      pirates
  in
  let hit_now = !shots > 0 in
  { planet;
    station;
    pirates;
    speed;
    energy = min 100 (if firing then f.energy - 4 else f.energy + 1);
    laser = (if firing then 6 else max 0 (f.laser - 1));
    kills = f.kills + killed;
    shield = f.shield - (!shots * 9);
    frames = f.frames + 1;
    hit = (if hit_now then 20 else max 0 (f.hit - 1)) }

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  match m.scene with
  | Title -> if space then Scene2d.go (Flying (new_flight ())) m else m
  | Flying f ->
      let f = step_flight computer.keyboard computer.keyboard.kspace f in
      if docking f then Scene2d.go (Docked f) m
      else if crashed f || f.shield <= 0 then Scene2d.go (Destroyed f) m
      else { m with scene = Flying f }
  | Docked _ | Destroyed _ -> if space then Scene2d.go Title m else m

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let green = rgb 90 255 110
let text (color : color) (size : number) (str : string) : shape = words color str |> Playground.scale size

(* Lave's planet: not polygons, in Elite or here -- a circle at its
 * distance, with a line across it for the equator. It is carried round
 * by your turns like everything else (a first version drew it at a
 * fixed place, and it hung in front of you whichever way you turned) *)
let planet_radius = 10000.

let planet (f : flight) : shape list =
  let p = f.planet in
  if p.z < near then []
  else
    let x, y = project p in
    let r = focal *. planet_radius /. p.z in
    [ circle (rgb 20 40 70) r |> move x y; line (rgb 60 110 160) (x -. r, y) (x +. r, y) ]

(* the scanner, Elite's own: every ship a dot on an ellipse where it
 * lies around you, on a stick as tall as it is above or below you *)
let scanner (screen : screen) (f : flight) : shape list =
  let cx = 0. and cy = screen.bottom +. 90. in
  let k = 0.06 in
  let blip (color : color) (p : vec) =
    let x = Basics.clamp (-200.) 200. (p.x *. k) and z = Basics.clamp (-70.) 70. (p.z *. k *. 0.35) in
    let y = Basics.clamp (-60.) 60. (p.y *. k *. 0.5) in
    [ line color (cx +. x, cy +. z) (cx +. x, cy +. z +. y); rectangle color 6. 4. |> move (cx +. x) (cy +. z +. y) ]
  in
  [ oval (rgb 0 40 0) 420. 150. |> move cx cy; line (rgb 0 90 0) (cx -. 210., cy) (cx +. 210., cy);
    line (rgb 0 90 0) (cx, cy -. 75.) (cx, cy +. 75.) ]
  @ blip (rgb 120 180 255) f.station.pos
  @ List.concat_map (fun (p : pirate) -> if p.alive then blip (rgb 255 90 90) p.ship.pos else []) f.pirates

let bar (label : string) (value : int) (color : color) (x : number) (y : number) : shape list =
  [ text white 1.8 label |> move (x -. 70.) y;
    rectangle (rgb 30 30 30) 160. 12. |> move (x +. 50.) y;
    rectangle color (1.6 *. float_of_int (max 0 value)) 10. |> move (x +. 50. -. (80. -. (0.8 *. float_of_int (max 0 value)))) y ]

let view_flight (screen : screen) (f : flight) : shape list =
  let ships =
    draw_hull (rgb 150 200 255) coriolis f.station.pos f.station.o
    @ List.concat_map (fun (p : pirate) -> if p.alive then draw_hull green sidewinder p.ship.pos p.ship.o else []) f.pirates
  in
  (* the slot: a letterbox on the square the nose points out of *)
  let slot =
    let c = add f.station.pos (times coriolis_size f.station.o.nose) in
    let corner a b = add c (add (times a f.station.o.side) (times b f.station.o.roof)) in
    let w = 44. and h = 14. in
    let pts = [ corner (-.w) (-.h); corner w (-.h); corner w h; corner (-.w) h ] in
    let rec pairs = function a :: (b :: _ as rest) -> (a, b) :: pairs rest | _ -> [] in
    List.filter_map
      (fun seg -> Option.map (fun (a, b) -> line (rgb 255 220 120) (project a) (project b)) (clip_near seg))
      (pairs (pts @ [ List.hd pts ]))
  in
  let laser =
    if f.laser > 0 then
      [ line (rgb 255 80 80) (screen.left +. 120., screen.bottom +. 200.) (0., 0.);
        line (rgb 255 80 80) (screen.right -. 120., screen.bottom +. 200.) (0., 0.) ]
    else []
  in
  let crosshair =
    [ line green (-20., 0.) (-6., 0.); line green (6., 0.) (20., 0.); line green (0., -20.) (0., -6.); line green (0., 6.) (0., 20.) ]
  in
  let flash = if f.hit > 12 then [ rectangle (rgb 120 0 0) screen.width screen.height |> fade 0.35 ] else [] in
  (rectangle black screen.width screen.height :: planet f)
  @ ships @ slot @ laser @ crosshair @ scanner screen f
  @ bar "SHIELD" f.shield (rgb 90 200 255) (screen.left +. 110.) (screen.bottom +. 150.)
  @ bar "ENERGY" f.energy (rgb 250 210 80) (screen.left +. 110.) (screen.bottom +. 125.)
  @ bar "SPEED" (int_of_float (f.speed *. 7.)) (rgb 120 255 120) (screen.left +. 110.) (screen.bottom +. 100.)
  @ [ text white 2. (Printf.sprintf "%s  %s  %s  TECH %d" lave.name lave.government lave.economy lave.tech)
      |> move 0. (screen.top -. 30.);
      text green 2. (Printf.sprintf "KILLS %d" f.kills) |> move (screen.right -. 100.) (screen.bottom +. 150.) ]
  @ flash

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  match m.scene with
  | Title ->
      (* the station alone, turning, as the attract mode's ship did *)
      let t = float_of_int m.frames *. 0.01 in
      let o = tidy (roll_by t (pitch_by 0.6 upright)) in
      (rectangle black screen.width screen.height :: draw_hull (rgb 150 200 255) coriolis (v 0. 0. 700.) o)
      @ [ text green 7. "TINY ELITE" |> move_y 330.;
          text white 2.4 "up/down: pitch   left/right: roll   w/s: speed   space: laser" |> move_y (-300.);
          text white 2.4 (Printf.sprintf "you are at %s: dock with the station, the slot turns" lave.name)
          |> move_y (-340.) ]
      @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-400.) ]
  | Flying f -> view_flight screen f
  | Docked f ->
      view_flight screen f
      @ [ rectangle black 800. 150. |> move_y 180. |> fade 0.7; text green 6. "DOCKED" |> move_y 200.;
          text white 2.6 (Printf.sprintf "%d pirates, and the slot on the first try" f.kills) |> move_y 140. ]
  | Destroyed f ->
      view_flight screen f
      @ [ rectangle black 800. 150. |> move_y 180. |> fade 0.7;
          text (rgb 255 90 90) 6. "GAME OVER" |> move_y 200. ]

let app = game view update initial_model

let main = Playground_platform.run_app app
