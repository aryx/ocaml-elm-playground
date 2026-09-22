(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Soldat (Michał Marcinkowski, "MM", 2002, Delphi, freeware; the
 * author's name and date from memory. Open-sourced later, now
 * Opensoldat, MIT: https://github.com/Soldat/soldat, "influenced by
 * the best of games such as Liero, Worms, Quake, Counter-Strike"): a
 * side-view deathmatch, soldiers running, jumping and flying on jet
 * boots over a map of polygons, shooting and throwing grenades, and
 * falling as ragdolls. You against two bots; the first to 5 kills wins.
 *
 *   a/d    run           w  jump; hold it in the air: the jets (fuel)
 *   mouse  aim           click (or space): shoot    q: a grenade
 *
 * The capstone of the physics plan
 * (docs/claude_notes/plan_physics_teaching.md, plan_games.md section
 * 17): nearly every piece of the engine, in one game.
 *
 * - The map and the soldiers are a Physics.world (phase 8's solver):
 *   the soldiers are upright boxes (phase 7: they don't tip over), run
 *   by setting their speed, pushed up by their jets against gravity.
 * - The bullets fly at 1500 pixels per second, 25 pixels a tick, more
 *   than a wall's or a soldier's width: tested by where they went
 *   (Physics.went_through, Collide's swept tests), not where they are
 *   -- tunneling, notes_2d_physics.md section 12.
 * - The grenades are bouncy bodies in the world, and their blast pushes
 *   the soldiers and the ragdolls away, weaker with the distance.
 * - A dead soldier becomes a ragdoll, Jakobsen's particles and sticks
 *   (Particles, the Hitman technique): 9 particles, a stick figure,
 *   falling, tumbling and lying on the map (Particles.keep_out).
 *
 * The bots see the target when nothing of the map is on the segment
 * between them (the same swept test), aim with a wobble (no Random: a
 * sine of the time, so every game is the same), run towards or away,
 * jump or fly when it's above them, and throw a grenade when it hides.
 *
 * What it uses: no kit; the Playground, Scene2d (the keys pressed), and
 * the Physics layer: a world ([world], [simulate]), [upright],
 * [immovable], [bouncy], [shot_from], [step], [went_through],
 * [touching]; underneath, physics/2d/'s Body, Integrate, Shape, Collide
 * (manifolds, the swept tests), Contact, Broadphase, Resolve, Solver,
 * and Particles for the ragdolls. Not Springs, Force.gravitation nor
 * Energy. The flag hitboxes draws what the physics sees.
 *
 * Left as exercises (the kit of plan_games.md section 17): a bigger map
 * and the camera following the player (Camera2d), more weapons (a
 * table: rate, speed, damage, spread), the bots' pathfinding over
 * waypoints, a map editor, two players over the network
 * (plan_networking_teaching.md), sounds.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The map *)
(*****************************************************************************)

let box (x : number) (y : number) (w : number) (h : number) : (number * number) list =
  [ (x - (w / 2.), y - (h / 2.)); (x + (w / 2.), y - (h / 2.)); (x + (w / 2.), y + (h / 2.)); (x - (w / 2.), y + (h / 2.)) ]

(* convex polygons, counterclockwise, in the screen's coordinates: hills,
 * the side walls, platforms, a bunker *)
let map_polygons : (number * number) list list =
  [ [ (-500., -500.); (-250., -500.); (-250., -330.); (-500., -300.) ];
    [ (-250., -500.); (0., -500.); (0., -380.); (-250., -330.) ];
    [ (0., -500.); (250., -500.); (250., -320.); (0., -380.) ];
    [ (250., -500.); (500., -500.); (500., -280.); (250., -320.) ];
    box (-510.) 0. 40. 1000.;
    box 510. 0. 40. 1000.;
    box (-300.) (-130.) 180. 20.;
    box 280. (-110.) 200. 20.;
    box 0. 50. 240. 20.;
    [ (-60., -390.); (60., -390.); (35., -310.); (-35., -310.) ];
    box (-320.) 220. 160. 20.;
    box 320. 230. 160. 20. ]

let map_bodies : Physics.body list =
  List.map (fun corners -> Physics.body (polygon (rgb 110 90 70) corners) |> Physics.immovable |> Physics.rough 0.6) map_polygons

let spawns = [ (-400., -250.); (400., -230.); (0., 100.); (-320., 260.); (320., 270.) ]

(* nothing of the map between a and b *)
let clear (a : number * number) (b : number * number) : bool =
  List.for_all (fun corners -> Collide.segment_polygon (a, b) corners = None) map_polygons

(*****************************************************************************)
(* The soldiers *)
(*****************************************************************************)

let soldier_body (color : color) ((x, y) : number * number) : Physics.body =
  Physics.body (rectangle color 22. 44.) |> Physics.at x y |> Physics.upright |> Physics.rough 0.2

type soldier = {
  name : string;
  color : color;
  human : bool;
  health : number;
  fuel : number;
  (* frames before the next shot, the next grenade *)
  reload : int;
  grenade_reload : int;
  (* where it aims, in degrees *)
  aim : number;
  (* None while alive; Some (frames since, its ragdoll) when dead *)
  dead : (int * Particles.particle array) option;
  kills : int;
}

(* what a soldier wants to do this frame: the player's keys, or a bot's
 * mind *)
type intent = { run : number; (* -1, 0, 1 *) jump : bool; jet : bool; shoot : bool; grenade : bool; aim : number }

type bullet = { b : Physics.body; owner : int; ttl : int }
type grenade = { fuse : int; thrower : int }
type blast = { x : number; y : number; age : int }

(* the world's bodies: the map's, then one per soldier (always there:
 * the solver knows bodies by their place), then the grenades' *)
type play = {
  world : Physics.world;
  soldiers : soldier array;
  bullets : bullet list;
  grenades : grenade list;
  blasts : blast list;
  frame : int;
}

type scene = Title | Playing of play | Over of string

type model = scene Scene2d.t

let n_map = List.length map_bodies
let body_of (p : play) (i : int) : Physics.body = List.nth p.world.bodies (n_map +.. i)

(* a dead soldier's body waits far away, out of everyone's way *)
let parked (color : color) : Physics.body = soldier_body color (0., 5000.) |> Physics.immovable

let start : play =
  let soldier name color human = { name; color; human; health = 100.; fuel = 100.; reload = 0; grenade_reload = 0; aim = 0.; dead = None; kills = 0 } in
  let soldiers = [| soldier "YOU" (rgb 220 60 50) true; soldier "BLUE" (rgb 60 110 220) false; soldier "GREEN" (rgb 60 170 80) false |] in
  let bodies = Array.to_list (Array.mapi (fun i s -> soldier_body s.color (List.nth spawns i)) soldiers) in
  { world = Physics.world (map_bodies @ bodies); soldiers; bullets = []; grenades = []; blasts = []; frame = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The ragdolls *)
(*****************************************************************************)

(* a stick figure: head, neck, hip, the knees and feet, the hands *)
let figure = [ (0., 18.); (0., 10.); (0., -8.); (-5., -16.); (-7., -24.); (5., -16.); (7., -24.); (-10., 0.); (10., 0.) ]
let bones : Particles.stick list =
  List.map
    (fun (a, b) -> { Particles.a; b; length = Vec2.length (Vec2.sub (List.nth figure b) (List.nth figure a)) })
    [ (0, 1); (1, 2); (2, 3); (3, 4); (2, 5); (5, 6); (1, 7); (1, 8); (0, 2) ]

(* the figure where the soldier fell, moving as it moved plus the hit's
 * push: its old positions a tick back along that velocity *)
let ragdoll (b : Physics.body) ((kx, ky) : number * number) : Particles.particle array =
  let (vx, vy) = (b.vx + kx, b.vy + ky) in
  Array.of_list
    (List.mapi
       (fun i (x, y) ->
         (* the head flies a bit faster: the figure starts tumbling *)
         let spin = if i = 0 then 1.5 else 1. in
         let pos = (b.x + x, b.y + y) in
         { (Particles.particle pos) with old = (fst pos - (vx * spin / 60.), snd pos - (vy / 60.)) })
       figure)

let gravity = 800.

let move_ragdoll (ps : Particles.particle array) : Particles.particle array =
  ps |> Particles.step ~drag:0.01 ~accel:(0., -.gravity) ~dt:(1. / 60.) |> Particles.relax ~iterations:5 bones |> Particles.keep_out map_polygons

(*****************************************************************************)
(* The bots *)
(*****************************************************************************)

let degrees (dx : number) (dy : number) : number = Float.atan2 dy dx * 180. / Float.pi

let bot (p : play) (i : int) : intent =
  let me = body_of p i in
  let others = List.filter (fun j -> j <> i && p.soldiers.(j).dead = None) [ 0; 1; 2 ] in
  let distance j = let b = body_of p j in Float.hypot (b.x - me.x) (b.y - me.y) in
  match List.sort (fun a b -> compare (distance a) (distance b)) others with
  | [] -> { run = 0.; jump = false; jet = false; shoot = false; grenade = false; aim = p.soldiers.(i).aim }
  | target :: _ ->
      let t = body_of p target in
      let dx = t.x - me.x and dy = t.y - me.y in
      let seen = clear (me.x, me.y + 10.) (t.x, t.y) in
      (* the aim wobbles, a sine of the time: no Random *)
      let wobble = 5. * sin ((float_of_int p.frame * 0.07) + float_of_int i) in
      (* nearer than 120: back off; farther than 260: go; in between,
       * strafe, changing sides every 1.5 s *)
      let strafe = if (p.frame /.. 90) mod 2 = 0 then 1. else -1. in
      let run = if Float.abs dx > 260. then Float.copy_sign 1. dx else if Float.abs dx < 120. then Float.copy_sign 1. (-.dx) else strafe in
      {
        run;
        jump = dy > 60. || (Float.abs me.vx < 20. && run <> 0.);
        jet = dy > 100. && p.soldiers.(i).fuel > 20.;
        shoot = seen;
        grenade = (not seen) && Float.abs dx < 450.;
        aim = (if seen then degrees dx dy + wobble else degrees dx (dy + 200.));
      }

let human (computer : computer) (scenes : model) (p : play) : intent =
  let k = computer.keyboard and m = computer.mouse in
  let me = body_of p 0 in
  let letter l = Set_.mem l k.keys in
  {
    run = (if letter "d" then 1. else 0.) - if letter "a" then 1. else 0.;
    jump = Scene2d.pressed (fun k -> Set_.mem "w" k.keys) scenes;
    jet = letter "w";
    shoot = m.mdown || k.kspace;
    grenade = Scene2d.pressed (fun k -> Set_.mem "q" k.keys) scenes;
    aim = degrees (m.mx - me.x) (m.my - me.y);
  }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* feet on something: a thin box under the soldier touching the map or
 * another soldier *)
let grounded (p : play) (b : Physics.body) : bool =
  let feet = Physics.body (rectangle white 18. 6.) |> Physics.at b.x (b.y - 24.) in
  List.exists (fun other -> other != b && Physics.touching feet other) p.world.bodies

(* the soldier's body driven by its intent, before the world's step *)
let drive (p : play) (s : soldier) (it : intent) (b : Physics.body) : soldier * Physics.body =
  let on_ground = grounded p b in
  (* running: towards the speed wanted, fast on the ground, slowly in
   * the air *)
  let wanted = it.run * 300. in
  let vx = b.vx + ((wanted - b.vx) * if on_ground then 0.3 else 0.05) in
  let vy = if it.jump && on_ground then 450. else b.vy in
  let jetting = it.jet && (not on_ground) && s.fuel > 0. in
  let b = b |> Physics.moving vx vy |> fun b -> if jetting then Physics.push 0. 1900. b else b in
  let fuel = if jetting then s.fuel - 1.2 else if on_ground then min 100. (s.fuel + 2.) else s.fuel in
  ({ s with fuel; aim = it.aim }, b)

(* a body pointing where the soldier aims: what shot_from fires from *)
let gun (s : soldier) (b : Physics.body) : Physics.body = b |> Physics.at b.x (b.y + 10.) |> Physics.pointing s.aim

let bullet_shape = circle (rgb 250 230 120) 2.5
let grenade_shape = circle (rgb 50 70 40) 7.

let blast_radius = 160.

let update_play (computer : computer) (scenes : model) (p : play) : play =
  let p = { p with frame = p.frame +.. 1 } in
  let bodies = Array.of_list p.world.bodies in
  let soldiers = Array.copy p.soldiers in
  let new_bullets = ref [] and new_grenades = ref [] in
  (* 1. the living soldiers act *)
  soldiers
  |> Array.iteri (fun i s ->
         if s.dead = None then (
           let it = if s.human then human computer scenes p else bot p i in
           let (s, b) = drive p s it bodies.(n_map +.. i) in
           bodies.(n_map +.. i) <- b;
           let s = if s.reload > 0 then { s with reload = s.reload -.. 1 } else s in
           let s = if s.grenade_reload > 0 then { s with grenade_reload = s.grenade_reload -.. 1 } else s in
           let s =
             if it.shoot && s.reload = 0 then (
               new_bullets := { b = Physics.body bullet_shape |> Physics.shot_from 1500. 26. (gun s b); owner = i; ttl = 60 } :: !new_bullets;
               { s with reload = 8 })
             else s
           in
           let s =
             if it.grenade && s.grenade_reload = 0 then (
               new_grenades :=
                 (Physics.body grenade_shape |> Physics.shot_from 550. 26. (gun s b) |> Physics.bouncy 0.5 |> Physics.rough 0.5 |> Physics.heavy 0.3, i)
                 :: !new_grenades;
               { s with grenade_reload = (if s.human then 60 else 240) })
             else s
           in
           soldiers.(i) <- s));
  (* 2. the world steps: the map, the soldiers, the grenades *)
  let world = Physics.simulate ~gravity { p.world with bodies = Array.to_list bodies @ List.map fst !new_grenades } in
  let bodies = Array.of_list world.bodies in
  let grenades = p.grenades @ List.map (fun (_, i) -> { fuse = 120; thrower = i }) !new_grenades in
  let damage = Array.make 3 0. and killer = Array.make 3 (-1) and knock = Array.make 3 (0., 0.) in
  let hurt i amount by (kx, ky) =
    if soldiers.(i).dead = None then (
      damage.(i) <- damage.(i) + amount;
      killer.(i) <- by;
      knock.(i) <- (fst knock.(i) + kx, snd knock.(i) + ky))
  in
  (* 3. the bullets: swept against the map and the soldiers *)
  let bullets =
    (p.bullets @ !new_bullets)
    |> List.filter_map (fun (bl : bullet) ->
           let b = bl.b |> Physics.fall 150. |> Physics.step in
           let hit_soldier =
             List.find_opt (fun i -> i <> bl.owner && soldiers.(i).dead = None && Physics.went_through b bodies.(n_map +.. i)) [ 0; 1; 2 ]
           in
           match hit_soldier with
           | Some i ->
               hurt i 20. bl.owner (b.vx * 0.15, b.vy * 0.15);
               None
           | None ->
               if bl.ttl = 0 || List.exists (Physics.went_through b) map_bodies then None
               else Some { bl with b; ttl = bl.ttl -.. 1 })
  in
  (* 4. the grenades' fuses, and their blasts *)
  let n_soldiers = 3 in
  let grenade_body k = bodies.(n_map +.. n_soldiers +.. k) in
  let blasts = ref (List.filter_map (fun bl -> if bl.age < 20 then Some { bl with age = bl.age +.. 1 } else None) p.blasts) in
  let ragdoll_kicks = ref [] in
  grenades
  |> List.iteri (fun k g ->
         if g.fuse = 0 then (
           let gb = grenade_body k in
           blasts := { x = gb.x; y = gb.y; age = 0 } :: !blasts;
           ragdoll_kicks := (gb.x, gb.y) :: !ragdoll_kicks;
           for i = 0 to n_soldiers -.. 1 do
             let sb = bodies.(n_map +.. i) in
             let d = Float.hypot (sb.x - gb.x) (sb.y - gb.y) in
             if d < blast_radius then (
               let f = 1. - (d / blast_radius) in
               let (ux, uy) = if d = 0. then (0., 1.) else ((sb.x - gb.x) / d, (sb.y - gb.y) / d) in
               hurt i (90. * f) g.thrower (ux * 600. * f, (uy * 600. * f) + (200. * f));
               if soldiers.(i).dead = None then bodies.(n_map +.. i) <- sb |> Physics.moving (sb.vx + (ux * 600. * f)) (sb.vy + (uy * 600. * f) + (200. * f)))
           done));
  (* the grenades that blew up leave the world (they're at its end) *)
  let keep = List.map (fun g -> g.fuse > 0) grenades in
  let bodies_list =
    Array.to_list bodies |> List.filteri (fun i _ -> i < n_map +.. n_soldiers || List.nth keep (i -.. n_map -.. n_soldiers))
  in
  let grenades = List.filter_map (fun g -> if g.fuse > 0 then Some { g with fuse = g.fuse -.. 1 } else None) grenades in
  let bodies = Array.of_list bodies_list in
  (* 5. the damage: deaths become ragdolls, the dead respawn after 2 s *)
  soldiers
  |> Array.iteri (fun i s ->
         match s.dead with
         | None ->
             let health = s.health - damage.(i) in
             if health <= 0. then (
               let b = bodies.(n_map +.. i) in
               soldiers.(i) <- { s with health = 0.; dead = Some (0, ragdoll b knock.(i)) };
               if killer.(i) >= 0 && killer.(i) <> i then
                 soldiers.(killer.(i)) <- { (soldiers.(killer.(i))) with kills = soldiers.(killer.(i)).kills +.. 1 };
               bodies.(n_map +.. i) <- parked s.color)
             else soldiers.(i) <- { s with health }
         | Some (n, ps) ->
             (* the blasts kick the ragdolls too: their old positions
              * moved back, away from the blast *)
             let ps =
               List.fold_left
                 (fun ps (gx, gy) ->
                   Array.map
                     (fun (q : Particles.particle) ->
                       let (x, y) = q.pos in
                       let d = Float.hypot (x - gx) (y - gy) in
                       if d < blast_radius && d > 0. then
                         let f = 12. * (1. - (d / blast_radius)) in
                         { q with old = (fst q.old - ((x - gx) / d * f), snd q.old - ((y - gy) / d * f)) }
                       else q)
                     ps)
                 ps !ragdoll_kicks
             in
             if n > 120 then (
               (* respawn at the spawn point farthest from the living *)
               let living = List.filter (fun j -> soldiers.(j).dead = None) [ 0; 1; 2 ] in
               let room (x, y) = List.fold_left (fun m j -> let b = bodies.(n_map +.. j) in min m (Float.hypot (b.x - x) (b.y - y))) infinity living in
               let spot = List.fold_left (fun best sp -> if room sp > room best then sp else best) (List.hd spawns) spawns in
               bodies.(n_map +.. i) <- soldier_body s.color spot;
               soldiers.(i) <- { s with dead = None; health = 100.; fuel = 100. })
             else soldiers.(i) <- { s with dead = Some (n +.. 1, move_ragdoll ps) });
  { p with world = { world with bodies = Array.to_list bodies }; soldiers; bullets; grenades; blasts = !blasts }

let winner (p : play) : soldier option = Array.to_list p.soldiers |> List.find_opt (fun s -> s.kills >= 5)

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let space = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title | Over _ -> if space then Scene2d.go (Playing start) scenes else scenes
  | Playing p -> (
      let p = update_play computer scenes p in
      match winner p with Some s -> Scene2d.go (Over s.name) scenes | None -> { scenes with scene = Playing p })

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let segment (color : color) (width : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 - x1) (y2 - y1)) width
  |> rotate (degrees (x2 - x1) (y2 - y1))
  |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let bar (color : color) (width : number) (fraction : number) (x : number) (y : number) : shape list =
  [ rectangle (rgb 40 40 40) width 4. |> move x y; rectangle color (width * max 0. fraction) 4. |> move (x - (width * (1. - max 0. fraction) / 2.)) y ]

let view_soldier (s : soldier) (b : Physics.body) : shape list =
  match s.dead with
  | Some (_, ps) ->
      List.map (fun (st : Particles.stick) -> segment s.color 4. ps.(st.a).pos ps.(st.b).pos) bones
      @ [ circle s.color 7. |> move (fst ps.(0).pos) (snd ps.(0).pos) ]
  | None ->
      (* the legs swing as it runs *)
      let swing = 25. * sin (b.x / 12.) in
      let leg angle = rectangle s.color 6. 20. |> move_y (-10.) |> rotate angle |> move b.x (b.y - 8.) in
      [ leg swing; leg (-.swing);
        rectangle s.color 18. 24. |> move b.x (b.y + 4.);
        circle s.color 8. |> move b.x (b.y + 22.);
        rectangle (rgb 40 40 40) 26. 4. |> move_x 13. |> rotate s.aim |> move b.x (b.y + 10.) ]
      @ bar (rgb 220 60 60) 30. (s.health / 100.) b.x (b.y + 38.)
      @ bar (rgb 240 200 60) 30. (s.fuel / 100.) b.x (b.y + 33.)

let view_play (computer : computer) (p : play) : shape list =
  let n_soldiers = 3 in
  List.map Physics.draw map_bodies
  @ List.concat (List.mapi (fun i s -> view_soldier s (body_of p i)) (Array.to_list p.soldiers))
  @ List.map (fun bl -> Physics.draw bl.b) p.bullets
  @ List.mapi (fun k _ -> Physics.draw (List.nth p.world.bodies (n_map +.. n_soldiers +.. k))) p.grenades
  @ List.map (fun bl -> circle orange (10. + (float_of_int bl.age * 7.)) |> fade (1. - (float_of_int bl.age / 20.)) |> move bl.x bl.y) p.blasts
  @ (if List.mem_assoc "hitboxes" computer.flags then List.map Physics.debug p.world.bodies else [])
  @ List.mapi
      (fun i s ->
        text s.color 2.5 (Printf.sprintf "%s %d" s.name s.kills) |> move (-300. + (300. * float_of_int i)) 460.)
      (Array.to_list p.soldiers)
  @ (if p.soldiers.(0).dead <> None then [ text white 3. "respawning..." |> move_y 380. ] else [])

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 120 160 200) screen.width screen.height
  ::
  (match model.scene with
  | Title ->
      List.map Physics.draw map_bodies
      @ [ text white 6. "TINY SOLDAT" |> move_y 300.;
          text white 2. "a/d run   w jump, hold w in the air: jets" |> move_y 200.;
          text white 2. "mouse aim   click or space shoot   q grenade" |> move_y 160.;
          text white 2. "you against two bots: first to 5 kills" |> move_y 120. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-50.) ]
  | Playing p -> view_play computer p
  | Over name ->
      List.map Physics.draw map_bodies
      @ [ text white 5. (if name = "YOU" then "YOU WIN!" else name ^ " WINS") |> move_y 200. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-50.) ])

let help =
  {|TinySoldat
  keys:  a/d    run              w      jump; held in the air: jets
         space  shoot            q      a grenade
  mouse: aim; click to shoot
  flags: hitboxes  draw what the physics sees
  e.g.   dune exec games/shmup/TinySoldat.exe -- hitboxes
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
