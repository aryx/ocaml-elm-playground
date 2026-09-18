(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Phase 1 verification for Minecraft_model.ml, per
 * docs/claude_notes/plan_tiny_minecraft.md: "run the world-generation +
 * block-management code standalone and sanity-check invariants...
 * rather than relying on visual inspection." A throwaway print-based
 * check (as the plan itself suggests), not a Testo suite -- this
 * project has no existing Testo usage to extend, and the invariants
 * below are simple enough that pulling in a test framework for them
 * would be more machinery than the thing being tested. *)

let check (name : string) (cond : bool) : unit =
  if cond then Printf.printf "OK   %s\n%!" name
  else begin
    Printf.printf "FAIL %s\n%!" name;
    exit 1
  end

let () =
  Random.self_init ();
  let m = Minecraft_model.create_world () in

  check "world is non-empty"
    (Hashtbl.length m.world > 0);
  check "shown is non-empty"
    (Hashtbl.length m.shown > 0);
  check "shown is a real subset of world (every shown position is in world)"
    (Hashtbl.fold (fun position _texture ok -> ok && Hashtbl.mem m.world position) m.shown true);
  check "every shown position is actually exposed"
    (Hashtbl.fold (fun position _texture ok -> ok && Minecraft_model.exposed m position) m.shown true);
  check "every exposed world position is shown (shown is complete, not just correct)"
    (Hashtbl.fold
       (fun position _texture ok -> ok && ((not (Minecraft_model.exposed m position)) || Hashtbl.mem m.shown position))
       m.world true);

  (* claude: a concrete, deliberately-constructed scenario (not
   * relying on world generation happening to produce one), so this
   * check is exact rather than "probably covered by the random
   * world": a 3x3x3 solid cube has exactly one fully-buried block (its
   * center), not shown; removing one of its 6 neighbors must expose
   * it, and check_neighbors must pick that up immediately. *)
  let m2 = Minecraft_model.create () in
  for x = -1 to 1 do
    for y = -1 to 1 do
      for z = -1 to 1 do
        Minecraft_model.add_block m2 (x, y, z) Minecraft_model.Stone
      done
    done
  done;
  check "the center of a solid 3x3x3 cube is not exposed"
    (not (Minecraft_model.exposed m2 (0, 0, 0)));
  check "the center of a solid 3x3x3 cube is not shown"
    (not (Hashtbl.mem m2.shown (0, 0, 0)));
  Minecraft_model.remove_block m2 (1, 0, 0);
  check "removing a neighbor exposes the center"
    (Minecraft_model.exposed m2 (0, 0, 0));
  check "removing a neighbor shows the center (check_neighbors ran)"
    (Hashtbl.mem m2.shown (0, 0, 0));
  Minecraft_model.add_block m2 (1, 0, 0) Minecraft_model.Stone;
  check "re-adding the neighbor re-buries the center"
    (not (Minecraft_model.exposed m2 (0, 0, 0)));
  check "re-adding the neighbor hides the center again (check_neighbors ran)"
    (not (Hashtbl.mem m2.shown (0, 0, 0)));

  (* hit_test: aim straight down the +x axis from outside the cube;
   * should hit the near face of the cube, with the empty cell just in
   * front of it as "previous". *)
  (match Minecraft_model.hit_test m2 ~position:(-5., 0., 0.) ~vector:(1., 0., 0.) () with
  | None -> check "hit_test finds the cube" false
  | Some (hit, previous) ->
      check "hit_test hits the cube's near face" (hit = (-1, 0, 0));
      check "hit_test's previous cell is just in front of the hit"
        (previous = Some (-2, 0, 0)));
  (match Minecraft_model.hit_test m2 ~position:(-5., 10., 0.) ~vector:(1., 0., 0.) () with
  | None -> check "hit_test finds nothing when aimed over the cube" true
  | Some _ -> check "hit_test finds nothing when aimed over the cube" false);

  Printf.printf "world: %d blocks, %d shown (exposed)\n%!" (Hashtbl.length m.world) (Hashtbl.length m.shown);
  print_endline "All Minecraft_model invariants hold.";

  (* claude: Minecraft_player, in a small world worked out by hand: a
   * floor of stone at y = -2 (its top at y = -1.5) for x, z in -5..5,
   * and a wall at z = -3 (y -1 and 0) for x in -5..5, in front of a
   * player starting at the origin, looking along -z at it:
   *
   *     y
   *     0  . . . W . . .       W: the wall (z = -3)
   *    -1  . . . W . . .       P: the player's eyes (0, 0, 0),
   *    -2  F F F F F F F          body from y 0 down to -1
   *        -6 -5 -4 -3 -2 -1 0  z  (P at z = 0)
   *)
  let w = Minecraft_model.create () in
  for x = -5 to 5 do
    for z = -5 to 5 do
      Minecraft_model.add_block w (x, -2, z) Minecraft_model.Stone
    done;
    Minecraft_model.add_block w (x, -1, -3) Minecraft_model.Stone;
    Minecraft_model.add_block w (x, 0, -3) Minecraft_model.Stone
  done;
  let none : Minecraft_player.input = { forward = 0; right = 0; jump = false } in
  (* [seconds] of frames at 60 fps *)
  let run ?(input = none) (seconds : float) (p : Minecraft_player.t) : Minecraft_player.t =
    let rec loop n p = if n = 0 then p else loop (n - 1) (Minecraft_player.step w ~dt:(1. /. 60.) input p) in
    loop (int_of_float (seconds *. 60.)) p
  in
  let y_of (p : Minecraft_player.t) = let (_, y, _) = p.position in y in
  let z_of (p : Minecraft_player.t) = let (_, _, z) = p.position in z in
  let close a b = Float.abs (a -. b) < 1e-6 in

  let (sx, sy, sz) = Minecraft_player.sight_vector Minecraft_player.initial in
  check "yaw 0 looks along -z" (close sx 0. && close sy 0. && close sz (-1.));
  let (sx, _, sz) = Minecraft_player.sight_vector { Minecraft_player.initial with yaw = 90. } in
  check "yaw 90 looks along +x (turning right)" (close sx 1. && close sz 0.);

  (* standing: gravity pulls, the floor pushes back; the eyes end up a
   * quarter block (pad) below where the body would just touch it *)
  let rest = run 2. Minecraft_player.initial in
  check "standing on the floor: doesn't fall through" (y_of rest > -0.5 && y_of rest < 0.);
  check "standing on the floor: no vertical speed" (rest.dy = 0.);

  let fallen = run 3. { Minecraft_player.initial with position = (0., 5., 0.) } in
  check "falling from 5 blocks up: lands on the floor" (close (y_of fallen) (y_of rest) && fallen.dy = 0.);

  (* a jump: one frame of space, then watch the height; the jump speed
   * is computed for a 1-block jump *)
  let rec highest n p best =
    if n = 0 then (best, p) else
    let p = Minecraft_player.step w ~dt:(1. /. 60.) none p in
    highest (n - 1) p (Float.max best (y_of p))
  in
  let jumped = Minecraft_player.step w ~dt:(1. /. 60.) { none with jump = true } rest in
  let (peak, landed) = highest 120 jumped (y_of jumped) in
  check "jumping: rises about 1 block" (peak -. y_of rest > 0.9 && peak -. y_of rest < 1.1);
  check "jumping: lands back" (close (y_of landed) (y_of rest));

  (* walking forward (along -z) for 3 s, 15 blocks' worth: stopped by
   * the wall at z = -3, the eyes a quarter block (pad) into the next
   * cell, so at -2.25 *)
  let walked = run ~input:{ none with forward = 1 } 3. rest in
  check "walking into a wall: stopped by it" (Float.abs (z_of walked -. -2.25) < 0.01);

  let hovering = run 1. { Minecraft_player.initial with position = (0., 3., 0.); flying = true } in
  check "flying: no gravity" (close (y_of hovering) 3.);
  let climbed = run ~input:{ none with forward = 1 } 0.2
      { Minecraft_player.initial with position = (0., 3., 0.); flying = true; pitch = 45. } in
  check "flying: forward while looking up climbs" (y_of climbed > 3.5);
  print_endline "All Minecraft_player checks hold."
