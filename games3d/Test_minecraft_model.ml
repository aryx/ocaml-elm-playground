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
  print_endline "All Minecraft_model invariants hold."
