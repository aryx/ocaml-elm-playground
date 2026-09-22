(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Gauntlet II (Atari Games, 1986; Ed Logg, who also wrote
 * Asteroids and Centipede, and Gauntlet the year before): a dungeon
 * seen from above, four heroes to pick from, monsters without end, and
 * a health bar that goes down by itself. Arrows walk, space shoots,
 * shift drinks a potion. Gauntlet II is the one that let all four
 * players be the same hero, and that talked at you: "Warrior needs
 * food, badly", "Elf shot the food!", "Beware, Death is near". Its
 * ancestor is Dandy (Atari 800, 1983), and its children are every
 * crawl with a spawner in it. (Names and dates from memory, to check.)
 *
 * Three ideas, and the first is the one to keep:
 *
 * 1. THE GENERATOR. The monsters are not a set, they are a *flow*: a
 *    tile that makes a new one every so often, for ever, until it is
 *    shot. So the room does not empty as you fight, it fills; killing
 *    what comes at you is losing slowly. Every crowd game since
 *    borrows this, and the design lesson is that the tap matters more
 *    than the water:
 *
 *      without generators          with them
 *      kill 20, room is empty      kill 20, twenty-two have arrived
 *
 * 2. HEALTH IS THE CLOCK. Nothing here is on a timer: the hero loses a
 *    point of health every few frames, whatever he does, and food is
 *    the only way to buy more. So every question in the game -- go
 *    round for the treasure? stand and fight? -- is the same question,
 *    how much time is it worth. In the arcade that clock was also the
 *    coin slot.
 *
 * 3. THE MONSTERS ARE DELIBERATELY STUPID, and the flag chase=field
 *    shows what they would be otherwise. By default each monster steps
 *    towards the hero along whichever axis it is furthest from, and
 *    tries the other one when that is a wall. With chase=field the
 *    game builds one Dijkstra flow field from the hero every frame
 *    (ai/Pathfind, through gamekits/rts' Orders -- the same "one search
 *    for a whole crowd" that moves TinyWarcraft2's peasants) and every
 *    monster walks down it; it costs one search a frame however many
 *    monsters there are.
 *
 *    The difference is not where one would guess. On an open floor, or
 *    round a single pillar, the greedy walk arrives too: trying the
 *    other axis when one is blocked is already a wall-follower. What
 *    tells them apart is a pen whose only way out faces *away* from
 *    the hero, because then walking towards him is walking into its
 *    back wall:
 *
 *        ####            three grunts, fifteen seconds, the hero left
 *        # m |  -->      greedy: 0 arrive, 231 pixels away on average
 *        ####            field:  3 arrive, 1 pixel away
 *         ^ towards the hero is into this wall
 *
 *    (tests/games/Unit_games.ml builds exactly that pen). A dungeon of
 *    rooms and corridors has few such traps, which is why the arcade
 *    could get away with the cheap rule -- and why its crowd, pressed
 *    against the near wall of a room, looks the way it does.
 *
 * What it uses: Tilemap (the dungeons, as strings), Camera2d (the
 * dungeon is bigger than the screen), Scene2d, Audio, ai/Pathfind
 * through gamekits/rts' Orders for the flow field. Not gamekits/maze: its
 * Grid_move locks a mover to the middle of a tile, which is what
 * Pac-Man wants and Gauntlet does not -- here everything walks in
 * eight directions and slides along the walls. Not Physics: nothing
 * bounces, and a monster that walks into you does not push you.
 *
 * Exercises: the second player (the arcade's real subject: two heroes
 * on one keyboard, and the shared food that makes friends fight); the
 * thief who steals a key and runs for the wall; the potion that only
 * clears what is on the screen, not the whole level; "It's a trap!" --
 * a floor tile that turns the walls into monsters; walls that deflect
 * shots; and a level read from a file, so the dungeon is data all the
 * way.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The dungeon *)
(*****************************************************************************)

(* '#' wall, 'b' a wall a shot breaks, 'D' a door (a key opens it),
 * 'K' key, 'F' food, 'P' potion, 'T' treasure, 'X' the way down,
 * '@' where the hero comes in, and the generators: 'g' grunts,
 * 'h' ghosts, 'd' demons, 'l' lobbers *)
let tile = 48.

let levels : string list list =
  [ [ "########################";
      "#@....#........#.......#";
      "#.....#...g....#...T...#";
      "#.....#........#.......#";
      "#..F..###.###D##.......#";
      "#........#.#...........#";
      "######.###.#####.#######";
      "#....#.....#....#......#";
      "#.K..#..h..#..F.#...P..#";
      "#....#.....#....#......#";
      "#....###.###....########";
      "#......#.#.............#";
      "####.###.#####.#########";
      "#..............#.......#";
      "#...T....g.....#...X...#";
      "#..............#.......#";
      "#..............D.......#";
      "########################" ];
    [ "########################";
      "#@...#....d....#...T...#";
      "#....#.........#.......#";
      "#..F.bbbb.###..b...l...#";
      "#....#....#.#..#.......#";
      "#.K..#..g.#.#..#########";
      "######....#.#..........#";
      "#....D....#.####.#####.#";
      "#....#....#....#.#...#.#";
      "#..h.#....b....#.#.P.#.#";
      "#....#....#....#.#...#.#";
      "#....######....#.###.#.#";
      "#.........F....#.....#.#";
      "########.#######.#####.#";
      "#......#.#....d#.......#";
      "#..T...#.#.....#...X...#";
      "#......#.......#.......#";
      "########################" ] ]

(*****************************************************************************)
(* The heroes *)
(*****************************************************************************)

(* The four, as numbers: the arcade's whole characterisation is a row
 * in a table, and it is enough to make them feel different. *)
type hero = {
  name : string;
  color : color;
  speed : number; (* pixels a frame *)
  shot_damage : int;
  armour : number; (* what a blow costs, times this *)
  shot_every : int; (* frames between shots *)
}

let heroes : hero list =
  [ { name = "WARRIOR"; color = rgb 220 70 60; speed = 2.6; shot_damage = 3; armour = 0.7; shot_every = 22 };
    { name = "VALKYRIE"; color = rgb 70 150 240; speed = 2.9; shot_damage = 2; armour = 0.6; shot_every = 18 };
    { name = "WIZARD"; color = rgb 200 90 220; speed = 2.6; shot_damage = 4; armour = 1.3; shot_every = 16 };
    { name = "ELF"; color = rgb 90 200 110; speed = 3.5; shot_damage = 2; armour = 1.0; shot_every = 14 } ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type kind =
  | Grunt (* walks at you and hits *)
  | Ghost (* faster, and dies on you: it drains and is gone *)
  | Demon (* stands off and breathes fire *)
  | Lobber (* throws over the walls, and backs away *)
  | Death (* nothing kills it but a potion, and it eats your clock *)

type monster = { kind : kind; mx : number; my : number; life : int; cool : int }
type shot = { sx : number; sy : number; vx : number; vy : number; mine : bool; power : int }
type generator = { gcell : int * int; gkind : kind; glife : int; gnext : int }

type game = {
  who : hero;
  level : int;
  map : Tilemap.t;
  x : number; (* the hero, in world pixels *)
  y : number;
  facing : number * number;
  cool : int; (* frames until he can shoot again *)
  health : int;
  keys : int;
  potions : int;
  score : int;
  monsters : monster list;
  gens : generator list;
  shots : shot list;
  cam : Camera2d.t;
  says : (string * int) option; (* what the voice is saying, and for how long *)
  seed : int;
  frames : int;
  field : bool; (* the flow field instead of the dumb chase *)
}

type scene = Choosing of int | Playing of game | Dead of int | Escaped of int
type model = { scenes : scene Scene2d.t; best : int }

let hero_r = 16.
let drain_every = 6 (* a point of health, so 10 a second *)

(* The view is zoomed in, which is what makes the dungeon scroll: at 2
 * the screen shows 500 pixels of it, ten tiles across, out of a level
 * of twenty-four by eighteen. Gauntlet's own selling point in 1986 was
 * that scroll -- smooth, and in all eight directions at once, which
 * arcade hardware of the time did in the display chip and a home
 * computer could not do at all. Here it is one number, and Camera2d
 * does the rest: [follow] eases after the hero, [clamp] stops the view
 * at the walls of the level (and centers a level smaller than the
 * screen). What you lose is the map, which is why there is a minimap. *)
let zoom = 2.

(*****************************************************************************)
(* The dungeon, read *)
(*****************************************************************************)

let solid (c : char) : bool = c = '#' || c = 'b' || c = 'D'
let is_generator (c : char) : bool = c = 'g' || c = 'h' || c = 'd' || c = 'l'
let kind_of_generator (c : char) : kind = match c with 'h' -> Ghost | 'd' -> Demon | 'l' -> Lobber | _ -> Grunt

(* a box of half-size [r] around (x, y) is clear of the walls *)
let free (map : Tilemap.t) (r : number) (x : number) (y : number) : bool =
  List.for_all
    (fun (dx, dy) -> match Tilemap.tile_at map (x + dx) (y + dy) with Some c -> not (solid c) | None -> false)
    [ (0. - r, 0. - r); (r, 0. - r); (0. - r, r); (r, r) ]

(* one step, sliding along a wall it cannot pass: x first, then y, so a
 * hero walking into a corner still moves along it *)
let walk (map : Tilemap.t) (r : number) ((x, y) : number * number) ((dx, dy) : number * number) : number * number =
  let x = if free map r (x + dx) y then x + dx else x in
  let y = if free map r x (y + dy) then y + dy else y in
  (x, y)

let cell_of (map : Tilemap.t) (x : number) (y : number) : int * int = Tilemap.cell map x y
let center_of (map : Tilemap.t) ((col, row) : int * int) : number * number = Tilemap.center map col row

let load (who : hero) (field : bool) (level : int) (carried : int * int * int) : game =
  let keys, potions, score = carried in
  let map = Tilemap.of_strings tile (List.nth levels level) in
  let x, y = match Tilemap.find map '@' with c :: _ -> center_of map c | [] -> (0., 0.) in
  let gens =
    List.concat_map
      (fun c ->
        List.map (fun cell -> { gcell = cell; gkind = kind_of_generator c; glife = 6; gnext = 90 }) (Tilemap.find map c))
      [ 'g'; 'h'; 'd'; 'l' ]
  in
  { who; level; map; x; y; facing = (0., -1.); cool = 0; health = 700; keys; potions; score; monsters = []; gens; shots = [];
    cam = { Camera2d.origin with zoom } |> Camera2d.look_at x y; says = Some (Printf.sprintf "%s ENTERS THE DUNGEON" who.name, 150); seed = 7 +.. level;
    frames = 0; field }

let initial_model = { scenes = Scene2d.start (Choosing 0); best = 0 }

(*****************************************************************************)
(* The voice *)
(*****************************************************************************)

let says (g : game) (what : string) : game = { g with says = Some (what, 150) }

(*****************************************************************************)
(* The monsters *)
(*****************************************************************************)

let monster_stats (k : kind) : number * int * int =
  (* speed, life, and what touching it costs *)
  match k with
  | Grunt -> (1.5, 3, 20)
  | Ghost -> (2.3, 1, 30)
  | Demon -> (1.1, 4, 25)
  | Lobber -> (1.2, 3, 20)
  | Death -> (1.0, 999, 200)

let monster_r = 15.

(* The dumb chase of the arcade: towards the hero along the axis it is
 * furthest from, and if that is a wall, along the other one. Nothing
 * looks ahead, which is why they pile up on the corners. *)
let greedy_step (g : game) (m : monster) (speed : number) : number * number =
  let dx = g.x - m.mx and dy = g.y - m.my in
  let sx = if Float.abs dx < 1. then 0. else if dx > 0. then speed else 0. - speed in
  let sy = if Float.abs dy < 1. then 0. else if dy > 0. then speed else 0. - speed in
  if Float.abs dx > Float.abs dy then
    if free g.map monster_r (m.mx + sx) m.my then (sx, 0.) else (0., sy)
  else if free g.map monster_r m.mx (m.my + sy) then (0., sy)
  else (sx, 0.)

(* The other way: one Dijkstra from the hero for the whole crowd
 * (Orders.field, gamekits/rts), and each monster walks to the next tile
 * down it. It costs one search a frame however many monsters there
 * are, which is the point of a field. *)
let field_step (g : game) (field : ((int * int) * number) list) (m : monster) (speed : number) : number * number =
  let walkable (col, row) = match Tilemap.get g.map col row with Some c -> not (solid c) | None -> false in
  match Orders.downhill ~walkable field (cell_of g.map m.mx m.my) with
  | None -> greedy_step g m speed
  | Some next ->
      let tx, ty = center_of g.map next in
      let dx = tx - m.mx and dy = ty - m.my in
      let d = Float.max 1e-9 (Float.hypot dx dy) in
      (speed * dx / d, speed * dy / d)

let step_monster (g : game) (field : ((int * int) * number) list option) (m : monster) : monster * shot list =
  let speed, _, _ = monster_stats m.kind in
  let dx, dy = match field with Some f -> field_step g f m speed | None -> greedy_step g m speed in
  (* a demon stops to breathe fire, a lobber backs away and throws over
   * the walls: the two that do not simply walk at you *)
  let far = Float.hypot (g.x - m.mx) (g.y - m.my) in
  let hold = (m.kind = Demon && far < 260.) || (m.kind = Lobber && far < 200.) in
  let dx, dy = if hold then (0. - (dx / 2.), 0. - (dy / 2.)) else (dx, dy) in
  let mx, my = walk g.map monster_r (m.mx, m.my) (dx, dy) in
  let m = { m with mx; my; cool = max 0 (m.cool -.. 1) } in
  if m.cool > 0 || not (m.kind = Demon || m.kind = Lobber) then (m, [])
  else if far > 420. then (m, [])
  else begin
    (* the demon's fire flies straight, the lobber's ball is slower and
     * goes over the walls (nothing stops it but the hero) *)
    let d = Float.max 1e-9 far in
    let speed = if m.kind = Demon then 6. else 4.5 in
    Audio.play Audio.laser;
    ({ m with cool = if m.kind = Demon then 90 else 130 },
     [ { sx = m.mx; sy = m.my; vx = speed * (g.x - m.mx) / d; vy = speed * (g.y - m.my) / d; mine = false; power = 1 } ])
  end

(*****************************************************************************)
(* The generators: the flow of monsters, and the tap *)
(*****************************************************************************)

let next_seed (seed : int) : int = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff

(* a generator makes a monster beside itself, as often as the level is
 * deep, until it is shot *)
let step_generators (g : game) : game =
  List.fold_left
    (fun g (gen : generator) ->
      if gen.gnext > 0 then { g with gens = { gen with gnext = gen.gnext -.. 1 } :: g.gens }
      else
        let seed = next_seed g.seed in
        let gx, gy = center_of g.map gen.gcell in
        let a = float_of_int (seed mod 360) * pi / 180. in
        let mx = gx + (34. * cos a) and my = gy + (34. * sin a) in
        let born = free g.map monster_r mx my && List.length g.monsters < 40 in
        let _, life, _ = monster_stats gen.gkind in
        { g with seed;
          gens = { gen with gnext = max 40 (150 -.. (20 *.. g.level)) } :: g.gens;
          monsters = (if born then { kind = gen.gkind; mx; my; life; cool = 60 } :: g.monsters else g.monsters) })
    { g with gens = [] } g.gens

(*****************************************************************************)
(* The hero *)
(*****************************************************************************)

let step_hero (computer : computer) (g : game) : game =
  let k = computer.keyboard in
  let dx, dy = to_xy k in
  let speed = g.who.speed in
  let x, y = walk g.map hero_r (g.x, g.y) (dx * speed, dy * speed) in
  let facing = if dx = 0. && dy = 0. then g.facing else (dx, dy) in
  let g = { g with x; y; facing; cool = max 0 (g.cool -.. 1) } in
  (* shooting: the shot flies the way he faces, and the hero of the
   * arcade could not shoot and walk in different directions *)
  if (not k.kspace) || g.cool > 0 then g
  else begin
    Audio.play Audio.blip;
    let fx, fy = g.facing in
    { g with cool = g.who.shot_every; shots = { sx = x; sy = y; vx = fx * 9.; vy = fy * 9.; mine = true; power = g.who.shot_damage } :: g.shots }
  end

(* what the hero walks over: the dungeon's items, taken out of the map
 * as they are taken *)
let pick_up (g : game) : game =
  let col, row = cell_of g.map g.x g.y in
  match Tilemap.get g.map col row with
  | Some 'F' ->
      Audio.play Audio.coin;
      says { g with map = Tilemap.set g.map col row '.'; health = min 999 (g.health +.. 250) } "FOOD"
  | Some 'K' ->
      Audio.play Audio.coin;
      says { g with map = Tilemap.set g.map col row '.'; keys = g.keys +.. 1 } "A KEY"
  | Some 'P' ->
      Audio.play Audio.coin;
      says { g with map = Tilemap.set g.map col row '.'; potions = g.potions +.. 1 } "A POTION"
  | Some 'T' ->
      Audio.play Audio.coin;
      { g with map = Tilemap.set g.map col row '.'; score = g.score +.. 100 }
  | _ -> (
      (* a door is opened from in front of it: it is a wall, so the
       * hero never stands on one *)
      let fx, fy = g.facing in
      let ahead = cell_of g.map (g.x + (fx * 34.)) (g.y + (fy * 34.)) in
      match Tilemap.get g.map (fst ahead) (snd ahead) with
      | Some 'D' when g.keys > 0 ->
          Audio.play Audio.hit;
          says { g with map = Tilemap.set g.map (fst ahead) (snd ahead) '.'; keys = g.keys -.. 1 } "THE DOOR IS OPEN"
      | _ -> g)

(* the potion of the arcade: everything on the level dies at once, and
 * the generators take a beating too *)
let drink (g : game) : game =
  if g.potions = 0 then g
  else begin
    Audio.play Audio.explosion;
    says
      { g with potions = g.potions -.. 1; monsters = []; score = g.score +.. (10 *.. List.length g.monsters);
        gens = List.filter_map (fun (gen : generator) -> if gen.glife <= 3 then None else Some { gen with glife = gen.glife -.. 3 }) g.gens }
      "MAGIC!"
  end

(*****************************************************************************)
(* What hits what *)
(*****************************************************************************)

let near (r : number) ((ax, ay) : number * number) ((bx, by) : number * number) : bool = Float.hypot (ax - bx) (ay - by) < r

(* A shot stops at a wall, and a breakable one goes with it. The
 * hero's shots also destroy the food, which is the game's most famous
 * line and its meanest rule: the thing keeping you alive is in the
 * line of fire. *)
let step_shots (g : game) : game =
  List.fold_left
    (fun g (s : shot) ->
      let sx = s.sx + s.vx and sy = s.sy + s.vy in
      let col, row = cell_of g.map sx sy in
      match Tilemap.get g.map col row with
      | None -> g
      | Some 'F' when s.mine ->
          Audio.play Audio.explosion;
          says { g with map = Tilemap.set g.map col row '.' } (Printf.sprintf "%s SHOT THE FOOD!" g.who.name)
      | Some 'b' when s.mine -> { g with map = Tilemap.set g.map col row '.' }
      | Some c when solid c -> g
      | Some c when is_generator c && s.mine -> (
          (* the tap, taking a shot *)
          match List.find_opt (fun (gen : generator) -> gen.gcell = (col, row)) g.gens with
          | None -> g
          | Some gen ->
              Audio.play Audio.hit;
              if gen.glife > s.power then { g with gens = List.map (fun (o : generator) -> if o == gen then { gen with glife = gen.glife -.. s.power } else o) g.gens }
              else
                says
                  { g with map = Tilemap.set g.map col row '.'; gens = List.filter (fun (o : generator) -> o != gen) g.gens;
                    score = g.score +.. 200 }
                  "A GENERATOR IS DOWN")
      | Some _ -> { g with shots = { s with sx; sy } :: g.shots })
    { g with shots = [] } g.shots

(* the hero's shots against the monsters, and the monsters' fire
 * against the hero *)
let shots_hit (g : game) : game =
  let mine, theirs = List.partition (fun (s : shot) -> s.mine) g.shots in
  let g =
    List.fold_left
      (fun g (s : shot) ->
        match List.find_opt (fun (m : monster) -> near 22. (s.sx, s.sy) (m.mx, m.my)) g.monsters with
        | None -> { g with shots = s :: g.shots }
        | Some m ->
            if m.kind = Death then { g with shots = s :: g.shots } (* nothing but magic touches it *)
            else if m.life > s.power then { g with monsters = List.map (fun (o : monster) -> if o == m then { m with life = m.life -.. s.power } else o) g.monsters }
            else begin
              Audio.play Audio.hit;
              { g with monsters = List.filter (fun (o : monster) -> o != m) g.monsters; score = g.score +.. 10 }
            end)
      { g with shots = [] } mine
  in
  List.fold_left
    (fun g (s : shot) ->
      if not (near 20. (s.sx, s.sy) (g.x, g.y)) then { g with shots = s :: g.shots }
      else begin
        Audio.play Audio.hit;
        { g with health = g.health -.. int_of_float (20. * g.who.armour) }
      end)
    g theirs

(* a monster that reaches you costs health; a ghost costs more and is
 * gone, which is what a ghost is *)
let monsters_hit (g : game) : game =
  List.fold_left
    (fun g (m : monster) ->
      if not (near 26. (m.mx, m.my) (g.x, g.y)) then { g with monsters = m :: g.monsters }
      else
        let _, _, bite = monster_stats m.kind in
        let hurt = g.health -.. int_of_float (float_of_int bite * g.who.armour) in
        Audio.play Audio.hit;
        if m.kind = Ghost then { g with health = hurt } else { g with health = hurt; monsters = m :: g.monsters })
    { g with monsters = [] } g.monsters

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  (* the clock: health goes down whatever happens, and the voice says
   * so when it gets low *)
  let g = if g.frames mod drain_every = 0 then { g with health = g.health -.. 1 } else g in
  let g =
    if g.health < 200 && g.frames mod 300 = 0 then says g (Printf.sprintf "%s NEEDS FOOD, BADLY" g.who.name)
    else match g.says with Some (_, 0) -> { g with says = None } | Some (w, n) -> { g with says = Some (w, n -.. 1) } | None -> g
  in
  let g = step_hero computer g in
  let g = if Scene2d.pressed (fun k -> k.kshift) scenes then drink g else g in
  let g = pick_up g in
  (* the flow field, once for the whole crowd, when the flag asks for it *)
  let field =
    if not g.field then None
    else
      let walkable (col, row) = match Tilemap.get g.map col row with Some c -> not (solid c) | None -> false in
      Some (Orders.field ~walkable (cell_of g.map g.x g.y))
  in
  let moved, fired = List.split (List.map (step_monster g field) g.monsters) in
  let g = { g with monsters = moved; shots = List.concat fired @ g.shots } in
  let g = g |> step_generators |> step_shots |> shots_hit |> monsters_hit in
  { g with cam = g.cam |> Camera2d.follow 0.18 g.x g.y |> Camera2d.clamp computer.screen (Tilemap.bounds g.map) }

let escaped (g : game) : bool = match Tilemap.tile_at g.map g.x g.y with Some 'X' -> true | _ -> false

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let go = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Choosing i ->
      let i = if Scene2d.pressed (fun k -> k.kright) scenes then (i +.. 1) mod List.length heroes else if Scene2d.pressed (fun k -> k.kleft) scenes then (i +.. List.length heroes -.. 1) mod List.length heroes else i in
      if go then
        let field = List.assoc_opt "chase" computer.flags = Some "field" in
        { model with scenes = Scene2d.go (Playing (load (List.nth heroes i) field 0 (0, 0, 0))) scenes }
      else { model with scenes = { scenes with scene = Choosing i } }
  | Playing g ->
      let g = update_game computer scenes g in
      if g.health <= 0 then { best = max model.best g.score; scenes = Scene2d.go (Dead g.score) scenes }
      else if escaped g then
        if g.level +.. 1 >= List.length levels then { best = max model.best (g.score +.. 1000); scenes = Scene2d.go (Escaped (g.score +.. 1000)) scenes }
        else { model with scenes = { scenes with scene = Playing (load g.who g.field (g.level +.. 1) (g.keys, g.potions, g.score +.. 500)) } }
      else { model with scenes = { scenes with scene = Playing g } }
  | Dead _ | Escaped _ -> if go || scenes.elapsed > 12. then { model with scenes = Scene2d.go (Choosing 0) scenes } else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let floor_color = rgb 42 40 52
let wall_color = rgb 96 92 110

let view_tile (c : char) : shape =
  match c with
  | '#' -> group [ square wall_color tile; square (rgb 120 116 138) (tile - 8.) ]
  | 'b' -> group [ square (rgb 110 90 70) tile; square (rgb 140 115 85) (tile - 10.) ]
  | 'D' -> group [ rectangle (rgb 190 150 60) tile (tile - 4.); rectangle (rgb 120 95 40) (tile - 16.) (tile - 18.) ]
  | 'K' -> group [ circle (rgb 240 210 80) 8. |> move_y 6.; rectangle (rgb 240 210 80) 5. 18. |> move_y (-6.); rectangle (rgb 240 210 80) 10. 4. |> move (5.) (-10.) ]
  | 'F' -> group [ circle (rgb 200 60 60) 13.; circle (rgb 240 120 110) 5. |> move (-4.) 4.; rectangle (rgb 90 180 90) 4. 8. |> move_y 13. ]
  | 'P' -> group [ circle (rgb 120 240 200) 12.; rectangle (rgb 120 240 200) 8. 8. |> move_y 12. ]
  | 'T' -> group [ rectangle (rgb 200 170 50) 26. 18.; rectangle (rgb 250 230 140) 26. 5. |> move_y 4. ]
  | 'X' -> group [ square (rgb 30 30 40) tile; square (rgb 90 220 255) (tile - 14.); text black 1.6 "DOWN" ]
  | 'g' -> group [ square (rgb 70 60 50) (tile - 6.); circle (rgb 180 140 60) 14.; circle (rgb 60 45 30) 6. ]
  | 'h' -> group [ square (rgb 50 60 70) (tile - 6.); circle (rgb 200 220 240) 14.; circle (rgb 40 50 60) 6. ]
  | 'd' -> group [ square (rgb 70 40 40) (tile - 6.); circle (rgb 230 90 60) 14.; circle (rgb 60 20 20) 6. ]
  | 'l' -> group [ square (rgb 50 60 40) (tile - 6.); circle (rgb 150 200 90) 14.; circle (rgb 40 50 30) 6. ]
  | _ -> square floor_color tile

let view_monster (m : monster) : shape =
  let body =
    match m.kind with
    | Grunt -> group [ circle (rgb 180 140 60) 13.; rectangle (rgb 120 90 40) 20. 8. |> move_y (-10.) ]
    | Ghost -> group [ circle (rgb 210 230 250) 13. |> fade 0.75; circle black 3. |> move (-4.) 3.; circle black 3. |> move 4. 3. ]
    | Demon -> group [ circle (rgb 230 90 60) 13.; triangle (rgb 250 160 60) 8. |> move_y 12. ]
    | Lobber -> group [ circle (rgb 150 200 90) 13.; circle (rgb 90 140 50) 6. |> move_y 6. ]
    | Death -> group [ circle (rgb 30 30 36) 16.; circle (rgb 220 60 60) 4. |> move (-5.) 3.; circle (rgb 220 60 60) 4. |> move 5. 3. ]
  in
  body |> move m.mx m.my

let view_hero (g : game) : shape list =
  let fx, fy = g.facing in
  [ circle g.who.color 15. |> move g.x g.y;
    circle (rgb 250 220 180) 8. |> move g.x (g.y + 4.);
    (* what he is facing, which is also where the next shot goes *)
    rectangle g.who.color 12. 6. |> rotate (radians_to_degrees (atan2 fy fx)) |> move (g.x + (fx * 16.)) (g.y + (fy * 16.)) ]

let view_world (g : game) : shape list =
  [ Tilemap.view view_tile g.map ]
  @ List.map view_monster g.monsters
  @ List.map (fun (s : shot) -> circle (if s.mine then rgb 250 240 150 else rgb 255 120 60) (if s.mine then 6. else 7.) |> move s.sx s.sy) g.shots
  @ view_hero g

(* The whole dungeon, small, in the corner. The map is already a list
 * of strings and a sprite is a list of strings, so Sprite.pixels draws
 * it in one shape, a pixel a tile, with the floor left out of the
 * palette so it stays transparent (TinyComanche3d draws its
 * terrain the same way). On top of it: where the monsters are, where
 * the hero is, and the part of the level the screen is showing. *)
let minimap_pixel = 5.

let minimap_palette =
  [ ('#', rgb 110 106 128); ('b', rgb 140 115 85); ('D', rgb 200 160 70); ('X', rgb 90 220 255); ('K', rgb 240 210 80); ('F', rgb 210 70 70);
    ('P', rgb 120 240 200); ('T', rgb 200 170 50); ('g', orange); ('h', orange); ('d', orange); ('l', orange) ]

let view_minimap (computer : computer) (g : game) : shape list =
  let cols = float_of_int (Tilemap.cols g.map) and rows = float_of_int (Tilemap.rows g.map) in
  let ox = 390. and oy = -400. in
  let at (col, row) : number * number =
    (ox + ((float_of_int col - ((cols - 1.) / 2.)) * minimap_pixel), oy + ((((rows - 1.) / 2.) - float_of_int row) * minimap_pixel))
  in
  let dot (cell : int * int) (color : color) (size : number) : shape =
    let x, y = at cell in
    rectangle color size size |> move x y
  in
  let seen = Camera2d.visible computer.screen g.cam in
  [ rectangle (rgb 10 10 16) ((cols * minimap_pixel) + 12.) ((rows * minimap_pixel) + 12.) |> move ox oy;
    Sprite.pixels minimap_pixel minimap_palette (Tilemap.to_strings g.map) |> move ox oy;
    (* what the screen is showing, in the dungeon *)
    rectangle white ((seen.right - seen.left) / tile * minimap_pixel) ((seen.top - seen.bottom) / tile * minimap_pixel)
    |> fade 0.14
    |> move (fst (at (cell_of g.map ((seen.left + seen.right) / 2.) ((seen.bottom + seen.top) / 2.))))
         (snd (at (cell_of g.map ((seen.left + seen.right) / 2.) ((seen.bottom + seen.top) / 2.)))) ]
  @ List.map (fun (m : monster) -> dot (cell_of g.map m.mx m.my) (rgb 230 90 80) 3.) g.monsters
  @ [ dot (cell_of g.map g.x g.y) g.who.color 5. ]

(* the arcade's bar along the top: who you are, how long you have left,
 * and what you are carrying *)
let view_hud (computer : computer) (g : game) : shape list =
  (* food can take him over the 700 he came in with, and the bar stops
   * at its frame rather than growing out of it *)
  let bar = Float.min 300. (300. * (float_of_int (max 0 g.health) / 700.)) in
  [ text g.who.color 2.2 g.who.name |> move (-400.) 470.;
    rectangle (rgb 60 60 70) 302. 20. |> move (-150.) 470.;
    rectangle (if g.health < 200 then rgb 230 70 60 else rgb 90 200 110) (Float.max 2. bar) 16. |> move (-300. + (bar / 2.)) 470.;
    text white 1.8 (Printf.sprintf "HEALTH %d" (max 0 g.health)) |> move (-150.) 442.;
    text (rgb 240 210 80) 2. (Printf.sprintf "KEYS %d" g.keys) |> move 120. 470.;
    text (rgb 120 240 200) 2. (Printf.sprintf "POTIONS %d" g.potions) |> move 260. 470.;
    text white 2. (Printf.sprintf "SCORE %d" g.score) |> move 410. 470.;
    text (rgb 150 150 170) 1.6 (Printf.sprintf "LEVEL %d" (g.level +.. 1)) |> move (-400.) 442. ]
  @ (match g.says with Some (what, _) -> [ text (rgb 250 240 150) 2.6 what |> move_y (-450.) ] | None -> [])
  @ view_minimap computer g
  @ if g.field then [ text (rgb 150 150 170) 1.6 "chase=field" |> move 410. 442. ] else []

let view_choosing (scenes : scene Scene2d.t) (model : model) (i : int) : shape list =
  [ text (rgb 250 240 150) 6. "TINY GAUNTLET II" |> move_y 330.; text white 2.2 "the monsters come out of the generators: shoot the tap, not the water" |> move_y 260. ]
  @ List.concat
      (List.mapi
         (fun j (h : hero) ->
           let x = -330. + (float_of_int j * 220.) in
           [ circle h.color (if j = i then 44. else 32.) |> move x 60.;
             circle (rgb 250 220 180) (if j = i then 22. else 16.) |> move x 70.;
             text (if j = i then white else rgb 130 130 150) 2.2 h.name |> move x (-30.);
             text (rgb 150 150 170) 1.6 (Printf.sprintf "speed %.1f  shot %d" h.speed h.shot_damage) |> move x (-70.) ])
         heroes)
  @ [ text white 2.2 "left/right choose      space enter the dungeon" |> move_y (-180.);
      text (rgb 150 150 170) 1.8 "arrows walk   space shoot   shift drink a potion" |> move_y (-230.);
      text (rgb 150 150 170) 1.8 "flag: chase=field   the monsters find their way round instead of into the walls" |> move_y (-280.) ]
  @ (if model.best > 0 then [ text white 2. (Printf.sprintf "BEST %d" model.best) |> move_y (-340.) ] else [])
  @ Scene2d.blink 1. scenes [ text (rgb 250 240 150) 3. "PRESS SPACE" |> move_y (-400.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and scenes = model.scenes in
  rectangle (rgb 18 18 24) screen.width screen.height
  ::
  (match scenes.scene with
  | Choosing i -> view_choosing scenes model i
  | Playing g -> Camera2d.view g.cam (view_world g) :: view_hud computer g
  | Dead score ->
      [ text (rgb 230 70 60) 6. "YOUR HEALTH RAN OUT" |> move_y 60.; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-30.) ]
      @ Scene2d.blink 1. scenes [ text (rgb 250 240 150) 3. "PRESS SPACE" |> move_y (-160.) ]
  | Escaped score ->
      [ text (rgb 120 240 160) 6. "YOU GOT OUT" |> move_y 60.; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-30.) ]
      @ Scene2d.blink 1. scenes [ text (rgb 250 240 150) 3. "PRESS SPACE" |> move_y (-160.) ])

let help =
  {|TinyGauntlet2
  keys:  arrows  walk        space  shoot (and start)
         shift   drink a potion
  flags: chase=field  the monsters follow a flow field instead of
                      walking straight at you and into the walls
  e.g.   dune exec games/rpg/TinyGauntlet2.exe -- chase=field
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
