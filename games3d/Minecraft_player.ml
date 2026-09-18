(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Minecraft_player.mli. *)

type t = { position : float * float * float; yaw : float; pitch : float; dy : float; flying : bool }

let initial = { position = (0., 0., 0.); yaw = 0.; pitch = 0.; dy = 0.; flying = false }

type input = { forward : int; right : int; jump : bool }

(*****************************************************************************)
(* Constants (the original's) *)
(*****************************************************************************)

let walking_speed = 5. (* blocks per second *)
let flying_speed = 15.
let gravity = 20. (* blocks per second, per second *)

(* the speed that reaches max_jump_height = 1 block: from v^2 = 2 g h
 * (the original derives it the same way, see its comment) *)
let jump_speed = sqrt (2. *. gravity *. 1.)
let terminal_velocity = 50.
let height = 2 (* blocks *)

let radians (degrees : float) : float = degrees *. Float.pi /. 180.

(*****************************************************************************)
(* Looking and moving *)
(*****************************************************************************)

(* [m] is 1 looking horizontally, 0 straight up or down: the horizontal
 * part of the unit vector *)
let sight_vector (p : t) : float * float * float =
  let m = cos (radians p.pitch) in
  (cos (radians (p.yaw -. 90.)) *. m, sin (radians p.pitch), sin (radians (p.yaw -. 90.)) *. m)

(* The direction of the player's motion, a unit vector (or zero).
 * Walking: in the horizontal plane, the angle of the keys (forward,
 * right, or a diagonal) added to the yaw. Flying: forward and back
 * follow the pitch too (fly up by looking up), strafing stays
 * horizontal. *)
let motion_vector (p : t) (input : input) : float * float * float =
  if input.forward = 0 && input.right = 0 then (0., 0., 0.)
  else
    (* the original's strafe: [0] is -1 forward, +1 back; [1] -1 left,
     * +1 right; atan2 of them is the keys' angle relative to the yaw:
     * -90 forward, 0 right, 90 back, 180 left *)
    let strafe = atan2 (float_of_int (-input.forward)) (float_of_int input.right) *. 180. /. Float.pi in
    let angle = radians (p.yaw +. strafe) in
    if p.flying then
      let (m, dy) = if input.right <> 0 then (1., 0.) else (cos (radians p.pitch), sin (radians p.pitch)) in
      let dy = if input.forward < 0 then -.dy else dy in
      (cos angle *. m, dy, sin angle *. m)
    else (cos angle, 0., sin angle)

(*****************************************************************************)
(* Collisions *)
(*****************************************************************************)
(* The world is a grid, so collision detection is simple: look at the
 * 6 cells next to the player's (for each of the 2 blocks of their
 * body), and if one is a block and the player overlaps it, push them
 * back out along that axis. [pad]: how much overlap counts; 0 would
 * collide as soon as touching a block, 0.49 would let the player sink
 * into the ground like into tall grass, 0.5 and more would let them
 * fall through it. *)
let pad = 0.25

let collide (world : Minecraft_model.t) (player : t) : t =
  let (x, y, z) = player.position in
  let p = [| x; y; z |] in
  let (nx, ny, nz) = Minecraft_model.normalize player.position in
  let np = [| nx; ny; nz |] in
  let dy = ref player.dy in
  Minecraft_model.faces
  |> List.iter (fun (fx, fy, fz) ->
         let face = [| fx; fy; fz |] in
         for i = 0 to 2 do
           if face.(i) <> 0 then begin
             (* how far into the neighboring cell, along this axis *)
             let d = (p.(i) -. float_of_int np.(i)) *. float_of_int face.(i) in
             if d >= pad then begin
               (* each block of the body, from the eyes down *)
               let hit = ref false in
               for body = 0 to height - 1 do
                 if not !hit then begin
                   let op = Array.copy np in
                   op.(1) <- op.(1) - body;
                   op.(i) <- op.(i) + face.(i);
                   if Hashtbl.mem world.world (op.(0), op.(1), op.(2)) then begin
                     hit := true;
                     p.(i) <- p.(i) -. ((d -. pad) *. float_of_int face.(i));
                     (* the ground or a ceiling: stop falling / rising *)
                     if fy <> 0 then dy := 0.
                   end
                 end
               done
             end
           end
         done);
  { player with position = (p.(0), p.(1), p.(2)); dy = !dy }

(*****************************************************************************)
(* One step *)
(*****************************************************************************)

let substep (world : Minecraft_model.t) (dt : float) (input : input) (player : t) : t =
  let speed = if player.flying then flying_speed else walking_speed in
  let (mx, my, mz) = motion_vector player input in
  let d = dt *. speed in
  let (dx, dy, dz) = (mx *. d, my *. d, mz *. d) in
  (* gravity: falling faster until the terminal velocity; jumping,
   * slowing down until falling *)
  let (dy, vy) =
    if player.flying then (dy, player.dy)
    else
      let vy = Float.max (player.dy -. (dt *. gravity)) (-.terminal_velocity) in
      (dy +. (vy *. dt), vy)
  in
  let (x, y, z) = player.position in
  collide world { player with position = (x +. dx, y +. dy, z +. dz); dy = vy }

let step (world : Minecraft_model.t) ~(dt : float) (input : input) (player : t) : t =
  (* a jump only from the ground (or a ceiling...), like the original *)
  let player = if input.jump && player.dy = 0. then { player with dy = jump_speed } else player in
  let dt = Float.min dt 0.2 in
  let substeps = 8 in
  let rec loop n player = if n = 0 then player else loop (n - 1) (substep world (dt /. float_of_int substeps) input player) in
  loop substeps player
