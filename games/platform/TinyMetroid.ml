(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Metroid (Nintendo R&D1 and Intelligent Systems,
 * 1986; Yoshio Sakamoto, Makoto Kano, Gunpei Yokoi): Samus Aran alone
 * in the caves of planet Zebes, and Kraid at the end of them. Arrows to
 * run, space to jump (held, higher), x to fire, up to aim up, c to arm
 * the missiles, down to curl into the morph ball (x then lays a bomb),
 * Enter for the map.
 *
 * Metroid's world is one piece, and it is closed: a tunnel too low to
 * walk into, a red door the beam bounces off, a ledge too high to jump
 * to, a floor of cracked blocks. Each is a lock, and somewhere in the
 * caves lies its key -- the morph ball, the missiles, the high jump
 * boots, the bombs -- each opening the way to the next. That order is
 * the game, and the new idea here:
 *
 *    start --[low tunnel: MORPH BALL]--> the missiles
 *          --[red door: MISSILES]------> the high jump boots
 *          --[high ledge: HIGH JUMP]---> the bombs
 *          --[cracked blocks: BOMBS]---> Kraid
 *
 *  - The locks are tiles, the keys abilities ([passable]): which tiles
 *    Samus can go through with what she has.
 *
 *  - A checker ([reachable], [progression]): every place Samus can get
 *    to with a set of abilities, found by a breadth-first search over
 *    her poses (a tile, standing or rolled up), with moves that are the
 *    game's own -- walking, falling, a jump of 3 tiles (5 with the
 *    boots) and across, curling up and standing. Then: take all the
 *    items that can be reached, search again, until nothing more. For
 *    this world it finds one item a round, in the order above, and
 *    Kraid only at the end: the world can be finished, and only that
 *    way. Change a tile of the map and a test says whether it still is.
 *
 *    The checker is cautious: a jump of 3 tiles where the game's jump
 *    rises a little more, and never further across than the jump stays
 *    that high long enough to go (tested). So it never promises a
 *    way the player can't take; but the player may find one it doesn't
 *    know about. That is a "sequence break", the speedrunners' art
 *    (Metroid's first players found the wall jump, then the bomb jump).
 *
 *  - The map (Enter): the world in areas, the ones Samus has been to
 *    drawn, the items seen there and not taken as dots. The original
 *    had none (its players drew theirs on paper); Super Metroid (1994)
 *    added it, and it is half of what "Metroidvania" means.
 *
 * What it uses: gamekits/platformer's Tile_move (Samus, the ball and
 * the crawlers against the rock, one pixel at a time), Tilemap (the
 * caves, changed by doors opened, blocks bombed, items taken; drawn
 * only where the camera looks), Camera2d (following Samus, clamped to
 * the world), Scene2d. Not Physics: the jump is TinyMario's.
 *
 * Exercises: the bomb jump (a bomb under the ball throws it up a tile
 * -- and the checker has to learn it, or the tests stay right while
 * the players go where it says they can't); the wall jump; the ice
 * beam, which freezes an enemy into a platform (another key: an enemy
 * as a step); blue doors that close behind you; more of Zebes (Norfair,
 * Ridley, Tourian and Mother Brain); the ending that depends on your
 * time -- the time is shown, the endings aren't.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The caves *)
(*****************************************************************************)

(* '#' rock, 'x' blocks only bombs break, 'R' a red door (missiles
 * open it); the items: 'o' the morph ball, 'm' the missiles, 'j' the
 * high jump boots, 'b' the bombs, 'e' an energy tank; 'S' Samus, 'z' a
 * crawler, 'f' a flyer, 'K' Kraid. Below, the start and the rooms of
 * the first three items; above, the corridor to the bombs and Kraid's
 * lair. *)
let world_text =
  [ "############################################################";
    "############################################################";
    "###############################################            #";
    "###############################################            #";
    "###############################################            #";
    "###############################################            #";
    "###############################################            #";
    "###############################################            #";
    "#                                        ###  #            #";
    "#              f                         ###  R            #";
    "#                                f       ###  R            #";
    "# b                                      xxx  R       K    #";
    "####xxx##################   ################################";
    "####   ##################  #################################";
    "#### e ##################   ################################";
    "#########################   ################################";
    "#########################   ################################";
    "##########################  ################################";
    "#########################   ################################";
    "#########################   ################################";
    "#########################   ################################";
    "#                 ####     #             #                 #";
    "#                 ####                   #                 #";
    "#                 ####                   #                 #";
    "#                 ####                   #                 #";
    "#                 ####   ##          m   #                 #";
    "#                 ####   ##         ###  R                 #";
    "#                 ####                   R                 #";
    "# o      S                      z        R       z     j   #";
    "############################################################";
    "############################################################";
    "############################################################" ]

let tile = 40.
let level = Tilemap.of_strings tile world_text

(* the caves to play: the living are not tiles *)
let start_map : Tilemap.t =
  List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') level (List.concat_map (Tilemap.find level) [ 'S'; 'z'; 'f'; 'K' ])

let solid (ch : char) : bool = ch = '#' || ch = 'x' || ch = 'R'

(*****************************************************************************)
(* The locks and the keys *)
(*****************************************************************************)

type ability = Morph_ball | Missiles | High_jump | Bombs

let ability_of (ch : char) : ability option =
  match ch with 'o' -> Some Morph_ball | 'm' -> Some Missiles | 'j' -> Some High_jump | 'b' -> Some Bombs | _ -> None

let name (a : ability) : string =
  match a with
  | Morph_ball -> "MORPH BALL: down to roll up"
  | Missiles -> "MISSILES: c to arm them, they open red doors"
  | High_jump -> "HIGH JUMP BOOTS"
  | Bombs -> "BOMBS: x in the ball"

(* The locks: what Samus can go through with the abilities [has] (for
 * the checker; the game itself says it tile by tile, a door opened by
 * a missile, a block by a bomb) *)
let passable (has : ability list) (ch : char) : bool =
  match ch with '#' -> false | 'R' -> List.mem Missiles has | 'x' -> List.mem Bombs has | _ -> true

(* how many tiles a jump rises, for the checker: the game's jump rises
 * a little more (see [jump_speed]) *)
let jump_tiles (has : ability list) : int = if List.mem High_jump has then 5 else 3

(* A pose: the tile of Samus's feet, and whether she is rolled up (one
 * tile high then, two standing) *)
type pose = { c : int; r : int; ball : bool }

(* Every pose Samus can get to from [start] with the abilities [has]: a
 * breadth-first search whose moves are the game's --
 *
 *   in the air              she falls, straight down
 *   on the ground           she walks a tile left or right, rolls up
 *                           (with the morph ball) or stands up (if
 *                           there's room above)
 *   on the ground, standing she jumps: 1 to [jump_tiles] up, and at
 *                           each height across, as far as the jump
 *                           stays that high long enough to go: 1 tile
 *                           at the top, 2 a tile lower, 3 below
 *
 *              X X X        <- 3 up    the jump from S: the X's, and
 *            X X X X X                 where they fall to
 *          X X X X X X X
 *                S
 *          # # # # # # #                                             *)
let reachable (m : Tilemap.t) (has : ability list) (start : pose) : pose list =
  let free c r = match Tilemap.get m c r with Some ch -> passable has ch | None -> false in
  let fits p = free p.c p.r && (p.ball || free p.c (p.r -.. 1)) in
  let moves p =
    if free p.c (p.r +.. 1) then [ { p with r = p.r +.. 1 } ]
    else
      let walk = [ { p with c = p.c -.. 1 }; { p with c = p.c +.. 1 } ] in
      let curl = if p.ball then [ { p with ball = false } ] else if List.mem Morph_ball has then [ { p with ball = true } ] else [] in
      let jump =
        if p.ball then []
        else
          let rec up h acc =
            let top = { p with r = p.r -.. h } in
            if h > jump_tiles has || not (fits top) then acc
            else
              (* across, while there's room *)
              let rec across q d n =
                let q = { q with c = q.c +.. d } in
                if n = 0 || not (fits q) then [] else q :: across q d (n -.. 1)
              in
              let far = min 3 (jump_tiles has -.. h +.. 1) in
              up (h +.. 1) ((top :: across top 1 far) @ across top (-1) far @ acc)
          in
          up 1 []
      in
      List.filter fits (walk @ curl @ jump)
  in
  let seen = Hashtbl.create 1000 in
  let rec bfs = function
    | [] -> ()
    | p :: rest ->
        let next = List.filter (fun q -> not (Hashtbl.mem seen q)) (moves p) in
        List.iter (fun q -> Hashtbl.replace seen q ()) next;
        bfs (rest @ next)
  in
  Hashtbl.replace seen start ();
  bfs [ start ];
  Hashtbl.fold (fun p () acc -> p :: acc) seen []

(* the tiles a pose covers: its feet's, and above them standing *)
let covers (p : pose) : (int * int) list = if p.ball then [ (p.c, p.r) ] else [ (p.c, p.r); (p.c, p.r -.. 1) ]

let start_pose : pose = match Tilemap.find level 'S' with (c, r) :: _ -> { c; r; ball = false } | [] -> { c = 1; r = 1; ball = false }

(* the items [has] reaches, on the map [m] *)
let items_reached (m : Tilemap.t) (has : ability list) : ability list =
  let tiles = List.concat_map covers (reachable m has start_pose) in
  List.sort_uniq compare (List.filter_map (fun (c, r) -> Option.bind (Tilemap.get m c r) ability_of) tiles)

(* The game as the checker plays it: all the items it can reach, taken,
 * again, until no new one: the abilities found, round after round.
 * For this world: [[Morph_ball]; [Missiles]; [High_jump]; [Bombs]] *)
let progression (m : Tilemap.t) : ability list list =
  let rec go has rounds =
    match List.filter (fun a -> not (List.mem a has)) (items_reached m has) with
    | [] -> List.rev rounds
    | found -> go (has @ found) (found :: rounds)
  in
  go [] []

(* whether [has] gets Samus to the tile [t] *)
let reaches (m : Tilemap.t) (has : ability list) (t : int * int) : bool =
  List.exists (fun p -> List.mem t (covers p)) (reachable m has start_pose)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

let stand_size = (24., 70.)
let ball_size = (24., 24.)

type samus = {
  x : number;
  y : number;
  vy : number;
  ball : bool;
  facing : number; (* 1. right, -1. left *)
  energy : int;
  tanks : int;
  missiles : int;
  armed : bool; (* missiles, not the beam *)
  has : ability list;
  hurt : int; (* frames of not being hurt again *)
}

type shot = { sx : number; sy : number; dx : number; dy : number; missile : bool; life : int }
type kind = Crawler | Flyer
type enemy = { kind : kind; ex : number; ey : number; dir : number; hp : int; home : number * number; gone : int (* frames since killed; 0 alive *) }
type boss = { kx : number; ky : number; khp : int; cool : int; flash : int }
type drop = { px : number; py : number; energy_drop : bool; life : int }

type game = {
  map : Tilemap.t;
  samus : samus;
  shots : shot list;
  spit : shot list; (* Kraid's *)
  bombs : (number * number * int) list; (* where, frames before it blows *)
  blasts : (number * number * int) list; (* where, frames since *)
  enemies : enemy list;
  boss : boss;
  drops : drop list;
  visited : (int * int) list; (* the areas of the map been to *)
  kills : int;
  message : string * int; (* and frames left *)
  frames : int;
}

type scene = Title | Playing of game * bool (* the map shown *) | Won of int | Game_over
type model = scene Scene2d.t

let center ((c, r) : int * int) : number * number = Tilemap.center level c r

(* standing on the floor of the tile (c, r), a body [h] high *)
let on_floor (h : number) ((c, r) : int * int) : number * number = let x, y = center (c, r) in (x, y - (tile / 2.) + (h / 2.))

let max_energy (s : samus) : int = 99 +.. (100 *.. s.tanks)

(* the areas of the map screen: 12 x 8 tiles *)
let area_of (x : number) (y : number) : int * int = let c, r = Tilemap.cell level x y in (c /.. 12, r /.. 8)

let new_game () : game =
  let x, y = on_floor (snd stand_size) (start_pose.c, start_pose.r) in
  let enemy kind (c, r) =
    let x, y = if kind = Crawler then on_floor 24. (c, r) else center (c, r) in
    { kind; ex = x; ey = y; dir = -1.; hp = 2; home = (x, y); gone = 0 }
  in
  let kx, ky = match Tilemap.find level 'K' with t :: _ -> on_floor 110. t | [] -> (0., 0.) in
  { map = start_map;
    samus = { x; y; vy = 0.; ball = false; facing = 1.; energy = 30; tanks = 0; missiles = 0; armed = false; has = []; hurt = 0 };
    shots = []; spit = []; bombs = []; blasts = [];
    enemies = List.map (enemy Crawler) (Tilemap.find level 'z') @ List.map (enemy Flyer) (Tilemap.find level 'f');
    boss = { kx; ky; khp = 8; cool = 90; flash = 0 };
    drops = []; visited = [ area_of x y ]; kills = 0; message = ("", 0); frames = 0 }

(*****************************************************************************)
(* Samus *)
(*****************************************************************************)

(* What the player does this frame, apart from the keyboard (the tests
 * play with it) *)
type controls = {
  dir : number; (* -1., 0., 1. *)
  jump : bool; (* held *)
  jump_now : bool; (* pressed *)
  fire_now : bool;
  aim_up : bool;
  down_now : bool;
  up_now : bool;
  switch_now : bool;
}

let idle = { dir = 0.; jump = false; jump_now = false; fire_now = false; aim_up = false; down_now = false; up_now = false; switch_now = false }

let gravity = 0.7

(* the jump: v^2 / 2g, a little less in steps of a frame, rises 3.2
 * tiles (129 pixels), 5.2 with the boots (208) -- the checker's 3 and
 * 5, and never 4 or 6 *)
let jump_speed (s : samus) : number = if List.mem High_jump s.has then 17.4 else 13.8

let size (s : samus) : number * number = if s.ball then ball_size else stand_size
let on_ground (m : Tilemap.t) (s : samus) : bool = Tile_move.on_ground solid m (size s) s.x s.y

let move_samus (c : controls) (m : Tilemap.t) (s : samus) : samus =
  let grounded = on_ground m s in
  (* rolling up, standing up: the feet stay where they are *)
  let s =
    if c.down_now && (not s.ball) && grounded && List.mem Morph_ball s.has then { s with ball = true; y = s.y - 35. + 12. }
    else if (c.up_now || c.jump_now) && s.ball then
      let up = { s with ball = false; y = s.y - 12. + 35. } in
      if Tile_move.hits solid m stand_size up.x up.y then s else up
    else s
  in
  let facing = if c.dir <> 0. then c.dir else s.facing in
  let (x, y), _ = Tile_move.move_by solid m (size s) (s.x, s.y) (c.dir * 4., 0.) in
  let vy =
    if c.jump_now && grounded && not s.ball then jump_speed s
    else if (not c.jump) && s.vy > 4. then 4. (* let go: the jump cut short *)
    else s.vy
  in
  let vy = max (-15.) (vy - gravity) in
  let (x, y), hit = Tile_move.move_by solid m (size s) (x, y) (0., vy) in
  { s with x; y; vy = (if hit then 0. else vy); facing; hurt = max 0 (s.hurt -.. 1) }

(* a door opened: the whole of it, the red tiles above and below *)
let open_door (m : Tilemap.t) (c : int) (r : int) : Tilemap.t =
  let rec clear m r d = if Tilemap.get m c r = Some 'R' then clear (Tilemap.set m c r ' ') (r +.. d) d else m in
  clear (clear m r (-1)) (r +.. 1) 1

let hurt (dmg : int) (from_x : number) (g : game) : game =
  let s = g.samus in
  if s.hurt > 0 then g
  else
    let push = if s.x >= from_x then 1. else -1. in
    let (x, y), _ = Tile_move.move_by solid g.map (size s) (s.x, s.y) (push * 30., 0.) in
    { g with samus = { s with energy = s.energy -.. dmg; hurt = 60; x; y; vy = (if s.ball then s.vy else 5.) } }

(* firing: a bomb in the ball, else a shot, a missile if armed *)
let fire (c : controls) (g : game) : game =
  let s = g.samus in
  if not c.fire_now then g
  else if s.ball then
    if List.mem Bombs s.has && List.length g.bombs < 3 then { g with bombs = (s.x, s.y, 40) :: g.bombs } else g
  else
    let missile = s.armed && s.missiles > 0 in
    let dx, dy, sx, sy = if c.aim_up then (0., 12., s.x, s.y + 40.) else (s.facing * 12., 0., s.x + (s.facing * 18.), s.y + 14.) in
    let samus = if missile then { s with missiles = s.missiles -.. 1; armed = s.missiles > 1 } else s in
    { g with samus; shots = { sx; sy; dx; dy; missile; life = (if missile then 50 else 22) } :: g.shots }

(* the items: taken when Samus covers their tile *)
let pick_up (g : game) : game =
  let s = g.samus in
  let w, h = size s in
  let tiles = List.sort_uniq compare (List.map (fun (dx, dy) -> Tilemap.cell g.map (s.x + dx) (s.y + dy)) [ (0., 0.); (0., h / 2. - 4.); (0., 4. - h / 2.); (w / 2. - 2., 0.); (2. - w / 2., 0.) ]) in
  List.fold_left
    (fun g (c, r) ->
      let s = g.samus in
      match Tilemap.get g.map c r with
      | Some 'e' ->
          let s = { s with tanks = s.tanks +.. 1 } in
          { g with map = Tilemap.set g.map c r ' '; samus = { s with energy = max_energy s }; message = ("ENERGY TANK", 180) }
      | Some ch -> (
          match ability_of ch with
          | Some a ->
              let s = { s with has = s.has @ [ a ] } in
              let s = if a = Missiles then { s with missiles = s.missiles +.. 10 } else s in
              { g with map = Tilemap.set g.map c r ' '; samus = s; message = (name a, 240) }
          | None -> g)
      | None -> g)
    g tiles

(*****************************************************************************)
(* The rest of Zebes *)
(*****************************************************************************)

let near (x1, y1) (x2, y2) (d : number) : bool = Float.abs (x1 - x2) < d && Float.abs (y1 - y2) < d

(* the shots: the rock stops them, a missile opens a red door, an enemy
 * takes one point from a beam and two from a missile; Kraid only feels
 * missiles *)
let move_shots (g : game) : game =
  List.fold_left
    (fun g (sh : shot) ->
      let sh = { sh with sx = sh.sx + sh.dx; sy = sh.sy + sh.dy; life = sh.life -.. 1 } in
      let c, r = Tilemap.cell g.map sh.sx sh.sy in
      match Tilemap.get g.map c r with
      | Some ch when solid ch -> if ch = 'R' && sh.missile then { g with map = open_door g.map c r } else g
      | _ when sh.life <= 0 -> g
      | _ -> (
          let b = g.boss in
          if b.khp > 0 && Float.abs (sh.sx - b.kx) < 45. && Float.abs (sh.sy - b.ky) < 55. then
            if sh.missile then { g with boss = { b with khp = b.khp -.. 1; flash = 8 } } else g (* the beam: a clink *)
          else
            match List.partition (fun e -> e.gone = 0 && near (e.ex, e.ey) (sh.sx, sh.sy) 22.) g.enemies with
            | e :: _, _ ->
                let hp = e.hp -.. if sh.missile then 2 else 1 in
                let enemies = List.map (fun x -> if x == e then { e with hp } else x) g.enemies in
                { g with enemies }
            | [], _ -> { g with shots = sh :: g.shots }))
    { g with shots = [] } g.shots

(* the bombs: they blow up the blocks around them, and the enemies *)
let move_bombs (g : game) : game =
  let blown, ticking = List.partition (fun (_, _, t) -> t <= 1) g.bombs in
  let g = { g with bombs = List.map (fun (x, y, t) -> (x, y, t -.. 1)) ticking; blasts = List.filter_map (fun (x, y, t) -> if t < 20 then Some (x, y, t +.. 1) else None) g.blasts } in
  List.fold_left
    (fun g (x, y, _) ->
      let c, r = Tilemap.cell g.map x y in
      let map =
        List.fold_left
          (fun m (dc, dr) -> if Tilemap.get m (c +.. dc) (r +.. dr) = Some 'x' then Tilemap.set m (c +.. dc) (r +.. dr) ' ' else m)
          g.map
          [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
      in
      let enemies = List.map (fun e -> if e.gone = 0 && near (e.ex, e.ey) (x, y) 50. then { e with hp = e.hp -.. 2 } else e) g.enemies in
      { g with map; enemies; blasts = (x, y, 0) :: g.blasts })
    g blown

(* the crawlers walk the floors, turning at walls and edges; the flyers
 * go back and forth, bobbing. The dead come back, once Samus is far. *)
let move_enemies (g : game) : game =
  let s = g.samus in
  let g, enemies =
    List.fold_left_map
      (fun g e ->
        if e.gone > 0 then
          if e.gone > 600 && not (near (fst e.home, snd e.home) (s.x, s.y) 700.) then (g, { e with ex = fst e.home; ey = snd e.home; hp = 2; gone = 0 })
          else (g, { e with gone = e.gone +.. 1 })
        else if e.hp <= 0 then
          let drop = { px = e.ex; py = e.ey; energy_drop = (g.kills mod 2 = 0 || not (List.mem Missiles s.has)); life = 400 } in
          ({ g with kills = g.kills +.. 1; drops = drop :: g.drops }, { e with gone = 1 })
        else
          let e =
            match e.kind with
            | Crawler ->
                let nx = e.ex + (e.dir * 1.2) in
                let wall = Tile_move.hits solid g.map (24., 24.) nx e.ey in
                let edge = not (Tile_move.hits solid g.map (4., 4.) (nx + (e.dir * 12.)) (e.ey - 16.)) in
                if wall || edge then { e with dir = -.e.dir } else { e with ex = nx }
            | Flyer ->
                let nx = e.ex + (e.dir * 2.) in
                let ey = snd e.home + (18. * sin (float_of_int g.frames / 20.)) in
                if Tile_move.hits solid g.map (30., 20.) nx ey then { e with dir = -.e.dir; ey } else { e with ex = nx; ey }
          in
          let g = if near (e.ex, e.ey) (s.x, s.y) (12. + (snd (size s) / 2.)) && Float.abs (e.ex - s.x) < 24. then hurt 8 e.ex g else g in
          (g, e))
      g g.enemies
  in
  { g with enemies }

(* Kraid: spits at Samus when she's near, hurts to touch *)
let move_boss (g : game) : game =
  let b = g.boss and s = g.samus in
  if b.khp <= 0 then g
  else
    let g = if Float.abs (s.x - b.kx) < 50. && Float.abs (s.y - b.ky) < 80. then hurt 15 b.kx g else g in
    let spit, cool =
      if b.cool > 0 then (g.spit, b.cool -.. 1)
      else if near (b.kx, b.ky) (s.x, s.y) 600. then
        let dx = s.x - b.kx and dy = s.y - (b.ky + 30.) in
        let d = max 1. (sqrt ((dx * dx) + (dy * dy))) in
        ({ sx = b.kx; sy = b.ky + 30.; dx = 5. * dx / d; dy = 5. * dy / d; missile = false; life = 150 } :: g.spit, 70)
      else (g.spit, 0)
    in
    let g, spit =
      List.fold_left
        (fun (g, acc) sh ->
          let sh = { sh with sx = sh.sx + sh.dx; sy = sh.sy + sh.dy; life = sh.life -.. 1 } in
          if near (sh.sx, sh.sy) (g.samus.x, g.samus.y) 26. then (hurt 12 sh.sx g, acc)
          else if sh.life <= 0 || Tile_move.hits solid g.map (6., 6.) sh.sx sh.sy then (g, acc)
          else (g, sh :: acc))
        (g, []) spit
    in
    { g with spit; boss = { b with cool; flash = max 0 (b.flash -.. 1) } }

let take_drops (g : game) : game =
  let s = g.samus in
  let taken, left = List.partition (fun d -> near (d.px, d.py) (s.x, s.y) 30.) g.drops in
  let s =
    List.fold_left
      (fun s d -> if d.energy_drop then { s with energy = min (max_energy s) (s.energy +.. 10) } else { s with missiles = min 30 (s.missiles +.. 3) })
      s taken
  in
  { g with samus = s; drops = List.filter_map (fun d -> if d.life > 1 then Some { d with life = d.life -.. 1 } else None) left }

let step (c : controls) (g : game) : game =
  let s = move_samus c g.map g.samus in
  let s = if c.switch_now && s.missiles > 0 && not s.ball then { s with armed = not s.armed } else s in
  let g = { g with samus = s; frames = g.frames +.. 1; message = (fst g.message, max 0 (snd g.message -.. 1)) } in
  let g = g |> fire c |> move_shots |> move_bombs |> move_enemies |> move_boss |> take_drops |> pick_up in
  let a = area_of g.samus.x g.samus.y in
  if List.mem a g.visited then g else { g with visited = a :: g.visited }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let initial_model : model = Scene2d.start Title

let controls (m : model) : controls =
  let k = m.keys in
  let pressed f = Scene2d.pressed f m in
  let letter l = pressed (fun k -> Set_.mem l k.keys) in
  { dir = (if k.kleft then -1. else if k.kright then 1. else 0.); jump = k.kspace; jump_now = pressed (fun k -> k.kspace);
    fire_now = letter "x"; aim_up = k.kup; down_now = pressed (fun k -> k.kdown); up_now = pressed (fun k -> k.kup);
    switch_now = letter "c" }

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let key f = Scene2d.pressed f m in
  match m.scene with
  | Title | Won _ | Game_over -> if key (fun k -> k.kspace) then Scene2d.go (match m.scene with Title -> Playing (new_game (), false) | _ -> Title) m else m
  | Playing (g, map_shown) ->
      if key (fun k -> k.kenter) then { m with scene = Playing (g, not map_shown) }
      else if map_shown then m
      else
        let g = step (controls m) g in
        if g.samus.energy <= 0 then Scene2d.go Game_over m
        else if g.boss.khp <= 0 then Scene2d.go (Won g.frames) m
        else { m with scene = Playing (g, false) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let item_color (ch : char) : color =
  match ch with 'o' -> rgb 250 120 200 | 'm' -> rgb 250 140 60 | 'j' -> rgb 120 220 250 | 'b' -> rgb 250 230 90 | _ -> rgb 250 80 80

let view_tile (frames : int) (ch : char) : shape =
  match ch with
  | '#' -> group [ square (rgb 30 60 110) tile; square (rgb 45 85 140) (tile - 8.); circle (rgb 35 70 125) 5. |> move (-6.) 5. ]
  | 'x' -> group [ square (rgb 90 80 70) tile; rectangle (rgb 50 45 40) 30. 3. |> rotate 35.; rectangle (rgb 50 45 40) 16. 3. |> rotate (-40.) |> move 6. 6. ]
  | 'R' -> group [ rectangle (rgb 90 30 30) tile tile; rectangle (rgb 230 60 60) 16. tile ]
  | 'o' | 'm' | 'j' | 'b' | 'e' ->
      let glow = 0.6 + (0.4 * sin (float_of_int frames / 8.)) in
      group [ rectangle (rgb 80 80 90) 30. 8. |> move_y (-16.); circle (item_color ch) 12. |> fade glow; circle white 4. |> move 3. 4. ]
  | _ -> group []

let view_samus (g : game) : shape list =
  let s = g.samus in
  if s.hurt > 0 && s.hurt /.. 4 mod 2 = 0 then []
  else if s.ball then
    [ group [ circle (rgb 230 120 40) 12.; rectangle (rgb 250 220 80) 24. 5. |> rotate (s.x * 3.) ] |> move s.x s.y ]
  else
    [ group
        [ rectangle (rgb 230 120 40) 24. 44. |> move_y (-8.);
          circle (rgb 230 120 40) 12. |> move_y 22.;
          rectangle (rgb 80 230 120) 12. 5. |> move (s.facing * 4.) 23.;
          rectangle (rgb 250 200 60) 18. 9. |> move (s.facing * 14.) 12.;
          rectangle (rgb 200 90 30) 8. 18. |> move (-6.) (-26.);
          rectangle (rgb 200 90 30) 8. 18. |> move 6. (-26.) ]
      |> move s.x s.y ]

let view_world (g : game) : shape list =
  let b = g.boss in
  List.concat_map
    (fun e ->
      if e.gone > 0 then []
      else
        match e.kind with
        | Crawler -> [ group [ circle (rgb 240 200 60) 12.; triangle (rgb 250 90 60) 7. |> move_y 12.; triangle (rgb 250 90 60) 7. |> move 9. 8.; triangle (rgb 250 90 60) 7. |> move (-9.) 8. ] |> move e.ex e.ey ]
        | Flyer -> [ group [ oval (rgb 90 200 110) 30. 16.; oval (rgb 60 150 80) 14. 22. |> move_y 8. |> rotate (8. * sin (float_of_int g.frames / 3.)); circle red 3. |> move (e.dir * 9.) 2. ] |> move e.ex e.ey ])
    g.enemies
  @ (if b.khp > 0 then
       [ group
           [ oval (if b.flash > 0 then white else rgb 90 160 70) 90. 110.;
             oval (rgb 150 200 110) 50. 60. |> move (-8.) (-15.);
             circle yellow 7. |> move (-18.) 30.; circle yellow 7. |> move 6. 30.;
             triangle (rgb 220 220 200) 10. |> move (-30.) 50.; triangle (rgb 220 220 200) 10. |> move 20. 52.;
             rectangle (rgb 60 60 60) 80. 6. |> move_y 68.; rectangle red (80. * float_of_int b.khp / 8.) 6. |> move_y 68. ]
         |> move b.kx b.ky ]
     else [])
  @ List.map (fun d -> (if d.energy_drop then circle (rgb 250 90 200) 7. else rectangle (rgb 250 140 60) 6. 14.) |> move d.px d.py) g.drops
  @ view_samus g
  @ List.map (fun (sh : shot) -> (if sh.missile then rectangle (rgb 250 140 60) 16. 6. |> rotate (if sh.dx = 0. then 90. else 0.) else circle (rgb 250 240 120) 5.) |> move sh.sx sh.sy) g.shots
  @ List.map (fun (sh : shot) -> circle (rgb 250 70 50) 8. |> move sh.sx sh.sy) g.spit
  @ List.map (fun (x, y, t) -> group [ circle black 7.; circle (if t /.. 5 mod 2 = 0 then red else rgb 80 0 0) 3. ] |> move x y) g.bombs
  @ List.map (fun (x, y, t) -> circle (rgb 250 200 80) (15. + (2.5 * float_of_int t)) |> fade (1. - (float_of_int t / 20.)) |> move x y) g.blasts

let view_hud (screen : screen) (g : game) : shape list =
  let s = g.samus in
  let top = screen.top - 30. and left = screen.left + 30. in
  [ text white 2.2 (Printf.sprintf "EN %02d" (s.energy mod 100)) |> move (left + 60.) top ]
  @ List.init s.tanks (fun i -> square (if s.energy > 99 *.. (i +.. 1) then rgb 250 90 200 else rgb 90 60 80) 14. |> move (left + 140. + (20. * float_of_int i)) top)
  @ (if List.mem Missiles s.has then [ text (if s.armed then rgb 250 140 60 else rgb 150 150 150) 2.2 (Printf.sprintf "MISSILE %d" s.missiles) |> move (left + 330.) top ] else [])
  @ [ text (rgb 150 160 190) 1.6 (Printf.sprintf "%d:%02d" (g.frames /.. 3600) (g.frames /.. 60 mod 60)) |> move (screen.right - 60.) top ]
  @ if snd g.message > 0 then [ text (rgb 250 230 140) 2.4 (fst g.message) |> move_y (top - 50.) ] else []

(* the map: the areas been to, Samus's blinking, the items seen and not
 * taken *)
let view_map (m : model) (g : game) : shape list =
  let cols = (Tilemap.cols g.map +.. 11) /.. 12 and rows = (Tilemap.rows g.map +.. 7) /.. 8 in
  let w = 110. and h = 74. in
  let at (ac, ar) = ((float_of_int ac - (float_of_int (cols -.. 1) / 2.)) * w, (float_of_int (rows -.. 1) / 2. - float_of_int ar) * h) in
  let here = area_of g.samus.x g.samus.y in
  [ rectangle black 700. 460. |> fade 0.85; text white 2.4 "MAP" |> move_y 200. ]
  @ List.concat_map
      (fun a ->
        let x, y = at a in
        let items =
          List.concat_map
            (fun r ->
              List.filter_map
                (fun c ->
                  match Tilemap.get g.map c r with
                  | Some ('o' | 'm' | 'j' | 'b' | 'e') when (c /.. 12, r /.. 8) = a -> Some (circle (rgb 250 230 90) 5. |> move (x + (float_of_int (c mod 12) - 5.5) * (w / 12.)) (y - (float_of_int (r mod 8) - 3.5) * (h / 8.)))
                  | _ -> None)
                (List.init (Tilemap.cols g.map) Fun.id))
            (List.init (Tilemap.rows g.map) Fun.id)
        in
        let color = if a = here && m.frames /.. 20 mod 2 = 0 then rgb 250 220 80 else rgb 60 110 200 in
        (rectangle color (w - 6.) (h - 6.) |> move x y) :: items)
      g.visited

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 8 8 20) screen.width screen.height
  ::
  (match m.scene with
  | Title ->
      [ text (rgb 250 140 60) 6. "TINY METROID" |> move_y 170.;
        text white 2.2 "Zebes: one world, closed by locks -- and their keys somewhere in it" |> move_y 80.;
        text white 2. "arrows run, space jumps, x fires, up aims up" |> move_y 20.;
        text white 2. "down rolls up (once you can), c arms missiles, Enter the map" |> move_y (-20.) ]
      @ Scene2d.blink 1. m [ text (rgb 250 220 60) 3. "PRESS SPACE" |> move_y (-160.) ]
  | Playing (g, map_shown) ->
      let cam = { Camera2d.origin with zoom = 1.5 } |> Camera2d.look_at g.samus.x (g.samus.y + 60.) |> Camera2d.clamp screen (Tilemap.bounds g.map) in
      let visible = Camera2d.visible screen cam in
      [ Camera2d.view cam (Tilemap.view_visible visible (view_tile g.frames) g.map :: view_world g) ]
      @ view_hud screen g
      @ if map_shown then view_map m g else []
  | Won frames ->
      [ text (rgb 120 230 140) 4. "KRAID IS DEFEATED" |> move_y 60.;
        text white 2.4 (Printf.sprintf "your time: %d:%02d" (frames /.. 3600) (frames /.. 60 mod 60)) |> move_y (-10.);
        text white 2. "space" |> move_y (-80.) ]
  | Game_over -> [ text (rgb 230 90 90) 4. "GAME OVER" |> move_y 30.; text white 2. "space" |> move_y (-40.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
