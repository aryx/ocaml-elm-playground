(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A port of Michael Fogleman's Minecraft clone in Python/Pyglet
 * (~/software-src/game/tiny-minecraft/main.py, see
 * docs/claude_notes/plan_tiny_minecraft.md): walk, jump, fly, and
 * remove and place blocks, in a generated world of ~85k blocks. Three
 * parts, like the original's classes: Minecraft_model, the world;
 * Minecraft_player, the player's physics; and this file, the rendering
 * and the controls (see "The game" below).
 *
 * Rendering ~50k exposed blocks rebuilt from scratch every frame (this
 * project's usual game3d style) measured at ~0.2 fps; the world is
 * built once instead, as cached chunks, see [chunk_shape] below and
 * docs/claude_notes/plan_opengl_perf.md.
 *
 * Atlas UV mapping: ~/software-src/game/tiny-minecraft/main.py's
 * texture.png (copied here) is a 4x4 grid of 64x64 cells, addressed
 * there as (col, gl_row) with gl_row 0 = the BOTTOM of the image
 * (pyglet/OpenGL's usual bottom-up texture convention) -- confirmed
 * empirically (see uv_rect_of_cell below) since this project's own UV
 * convention is the opposite (v=0 = TOP of the image file, same as
 * textured_quad/textured_cube already use, verified pixel-identical
 * against native's checker.png test). *)
open Basics (* elm-core: float +, -, *, /, clamp *)
open Playground3d

let atlas_src = "games3d/texture.png"
let atlas_n = 4

(* claude: converts the original's (col, gl_row) cell addressing (see
 * this file's header comment) to a (u0,v0)-(u1,v1) rect in this
 * project's own v=0-is-top convention: file_row = (atlas_n-1) - gl_row. *)
let uv_rect_of_cell (col, gl_row) =
  let file_row = atlas_n -.. 1 -.. gl_row in
  let m = 1. /. float_of_int atlas_n in
  let u0 = float_of_int col *. m and v0 = float_of_int file_row *. m in
  (u0, v0, u0 +. m, v0 +. m)

(* the original's GRASS/SAND/BRICK/STONE tex_coords(top, bottom, side)
 * calls, as plain (col, gl_row) cell tuples -- kept exactly as the
 * original's literal values for easy side-by-side comparison. *)
let atlas_cells_of_block : Minecraft_model.block -> (int * int) * (int * int) * (int * int) = function
  | Grass -> ((1, 0), (0, 1), (0, 0)) (* top, bottom, side *)
  | Sand -> ((1, 1), (1, 1), (1, 1))
  | Brick -> ((2, 0), (2, 0), (2, 0))
  | Stone -> ((2, 1), (2, 1), (2, 1))

(* claude: box_faces (the shared 6-corners-per-face builder box/cube/
 * textured_cube use) is deliberately kept private to Playground3d.ml
 * -- see its own doc comment there ("exposed as a real primitive
 * rather than leaving box_faces private" is about box itself, not
 * box_faces). Per plan_tiny_minecraft.md's own design decision ("A
 * textured_box_faces helper local to the Minecraft port, not a new
 * library primitive"), this is a small local duplicate rather than a
 * new addition to the shared library -- same corner points/winding as
 * box_faces (CCW as seen from outside, for correct backface culling),
 * just paired with an atlas sub-rect per face instead of textured_cube's
 * single full-image UV rect. *)
let block_faces (size : number) : (number * number * number) list list =
  let h = size / 2. in
  let p000 = (-.h, -.h, -.h)
  and p001 = (-.h, -.h, h)
  and p010 = (-.h, h, -.h)
  and p011 = (-.h, h, h)
  and p100 = (h, -.h, -.h)
  and p101 = (h, -.h, h)
  and p110 = (h, h, -.h)
  and p111 = (h, h, h) in
  [ [ p010; p011; p111; p110 ] (* +Y, top *)
  ; [ p000; p100; p101; p001 ] (* -Y, bottom *)
  ; [ p100; p110; p111; p101 ] (* +X, side *)
  ; [ p001; p011; p010; p000 ] (* -X, side *)
  ; [ p001; p101; p111; p011 ] (* +Z, side *)
  ; [ p000; p010; p110; p100 ] (* -Z, side *)
  ]

let textured_face (cell : int * int) = function
  | [ p0; p1; p2; p3 ] ->
      let (u0, v0, u1, v1) = uv_rect_of_cell cell in
      { alpha = 1.; form = TexturedPolygon3d (atlas_src, [ (p0, (u0, v0)); (p1, (u1, v0)); (p2, (u1, v1)); (p3, (u0, v1)) ]) }
  | _ -> assert false

(* claude: the direction of each of block_faces's faces, in the same
 * order: the neighbor on that side *)
let face_directions : Minecraft_model.pos list = [ (0, 1, 0); (0, -1, 0); (1, 0, 0); (-1, 0, 0); (0, 0, 1); (0, 0, -1) ]

(* claude: hidden-face culling -- only the faces with no block in front
 * of them, i.e. the ones touching air: a face pressed against a
 * neighbor can never be seen. Most shown blocks have 1 or 2 faces left
 * (the ground: just its top), so this divides the geometry by several.
 * The original Python version doesn't do it (the GPU copes), but it
 * costs nothing (6 lookups per block, once) and every backend gains.
 * Not the same thing as Minecraft_model.exposed, which decides whether
 * a block has at least one such face, i.e. whether it's in [shown] at
 * all. Set to false to see the difference (-debug logs the vertex
 * counts). The next step, merging neighboring coplanar faces into big
 * rectangles, is "greedy meshing": M. Lysenko, "Meshing in a Minecraft
 * Game", 0fps.net, 2012, which also starts from this culling. *)
let hidden_face_culling = true

let block_shape (m : Minecraft_model.t) ((x, y, z) : Minecraft_model.pos) (block : Minecraft_model.block) : shape3d =
  let (top, bottom, side) = atlas_cells_of_block block in
  let cells = [ top; bottom; side; side; side; side ] in
  let hidden (dx, dy, dz) = hidden_face_culling && Hashtbl.mem m.world (x +.. dx, y +.. dy, z +.. dz) in
  List.combine (List.combine (block_faces 1.) cells) face_directions
  |> List.filter_map (fun ((face, cell), dir) -> if hidden dir then None else Some (textured_face cell face))
  |> group3d
  |> move3d (float_of_int x) (float_of_int y) (float_of_int z)

(* claude: the world as chunks, one per sector (a 16x16 column, see
 * Minecraft_model.sectorize), each a Playground3d.cached3d built once,
 * here: the GPU backends upload each chunk once and on later frames
 * only draw it again (see docs/claude_notes/plan_opengl_perf.md), so
 * view's only work is returning this list. Rebuilding every block's
 * shape in view instead, every frame, took seconds per frame. Chunks
 * rather than one cached3d for the whole world so that an edit only
 * rebuilds the chunks it touches (see rebuild_chunks_around). *)
let chunk_shape (m : Minecraft_model.t) (sector : Minecraft_model.pos) : shape3d =
  let positions = match Hashtbl.find_opt m.sectors sector with Some ps -> !ps | None -> [] in
  positions
  |> List.filter_map (fun pos -> Hashtbl.find_opt m.shown pos |> Option.map (fun block -> block_shape m pos block))
  |> cached3d

(*****************************************************************************)
(* The world *)
(*****************************************************************************)
(* claude: the world, and its chunks, are the one mutable part of this
 * game, like the original's Model object: Minecraft_model's hash
 * tables, changed in place by add_block/remove_block, and [chunks],
 * sector -> its cached3d. The rest of the state (the player, what's
 * selected) is an ordinary immutable model, below. *)

let world = Minecraft_model.create_world ()
let chunks : (Minecraft_model.pos, shape3d) Hashtbl.t = Hashtbl.create 128
let () = Hashtbl.iter (fun sector _ -> Hashtbl.replace chunks sector (chunk_shape world sector)) world.sectors

(* claude: after an edit at [pos], the chunks that may look different:
 * [pos]'s own, and those of its 6 neighbors, whose exposed faces
 * changed (a neighbor can be in the next sector). Each gets a new
 * cached3d; the GPU backends free the old one's buffers by themselves,
 * since view stops returning it (Mesh_cache's sweep). *)
let rebuild_chunks_around ((x, y, z) : Minecraft_model.pos) : unit =
  (x, y, z) :: List.map (fun (dx, dy, dz) -> (x +.. dx, y +.. dy, z +.. dz)) Minecraft_model.faces
  |> List.map Minecraft_model.sectorize
  |> List.sort_uniq compare
  |> List.iter (fun sector -> Hashtbl.replace chunks sector (chunk_shape world sector))

(*****************************************************************************)
(* The game *)
(*****************************************************************************)
(* Controls (the original's, except the mouse, see [look]):
 *  - W/A/S/D: walk; space: jump; Tab: fly or walk (flying, look up or
 *    down to go up or down);
 *  - the mouse, or the arrow keys: look around;
 *  - left click: remove the block under the crosshair (not stone);
 *    right click: place one in front of it; 1/2/3: brick, grass, sand. *)

type model = {
  player : Minecraft_player.t;
  (* the arrow keys' part of where the player looks (see [look]) *)
  turn_yaw : number;
  turn_pitch : number;
  (* what a right click places *)
  block : Minecraft_model.block;
  (* the previous frame's time, buttons and Tab key: to know how much
   * time passed, and to act once per press rather than on every frame
   * a button is held *)
  last_time : number option;
  was_down : bool;
  was_right_down : bool;
  was_tab : bool;
}

let initial : model =
  {
    player = Minecraft_player.initial;
    turn_yaw = 0.;
    turn_pitch = 0.;
    block = Brick;
    last_time = None;
    was_down = false;
    was_right_down = false;
    was_tab = false;
  }

let inventory : (string * Minecraft_model.block) list = [ ("1", Brick); ("2", Grass); ("3", Sand) ]

(* claude: where the player looks. The original captures the mouse
 * (an invisible cursor that can move forever, reporting only how much
 * it moved); Playground.mouse is an absolute position in the window,
 * so here the mouse's offset from the window's center adds to the
 * direction, up to 90 degrees left or right at the window's edges (60
 * up or down), and the arrow keys turn further. *)
let look (computer : Playground.computer) (m : model) : number * number =
  let mouse_yaw = computer.mouse.mx / (computer.screen.width / 2.) * 90. in
  let mouse_pitch = computer.mouse.my / (computer.screen.height / 2.) * 60. in
  (m.turn_yaw + mouse_yaw, clamp (-89.) 89. (m.turn_pitch + mouse_pitch))

let key_down (computer : Playground.computer) (key : string) : bool = Set_.mem key computer.keyboard.keys

(* the block under the crosshair, and the empty cell in front of it *)
let target (m : model) = Minecraft_model.hit_test world ~position:m.player.position ~vector:(Minecraft_player.sight_vector m.player) ()

let edit (computer : Playground.computer) (m : model) : unit =
  let clicked = computer.mouse.mdown && not m.was_down in
  let right_clicked = computer.mouse.mrdown && not m.was_right_down in
  match target m with
  | Some (block_pos, _) when clicked && Hashtbl.find world.world block_pos <> Minecraft_model.Stone ->
      Minecraft_model.remove_block world block_pos;
      rebuild_chunks_around block_pos
  | Some (_, Some empty_pos) when right_clicked ->
      Minecraft_model.add_block world empty_pos m.block;
      rebuild_chunks_around empty_pos
  | _ -> ()

let update (computer : Playground.computer) (m : model) : model =
  let kb = computer.keyboard in
  let bool_int b = if b then 1 else 0 in
  (* -1., 0. or 1., from two opposite keys *)
  let axis (plus : bool) (minus : bool) : number = float_of_int (bool_int plus -.. bool_int minus) in
  (* the arrow keys turn (degrees per frame) *)
  let turn_yaw = m.turn_yaw + (2. * axis kb.kright kb.kleft) in
  let turn_pitch = clamp (-89.) 89. (m.turn_pitch + (2. * axis kb.kup kb.kdown)) in
  let m = { m with turn_yaw; turn_pitch } in
  let (yaw, pitch) = look computer m in
  let tab = key_down computer "tab" || key_down computer "Tab" in
  let flying = if tab && not m.was_tab then not m.player.flying else m.player.flying in
  let player = { m.player with yaw; pitch; flying } in
  let block = List.fold_left (fun b (key, block) -> if key_down computer key then block else b) m.block inventory in
  let (Time now) = computer.time in
  let dt = match m.last_time with Some last -> now - last | None -> 0. in
  let input : Minecraft_player.input =
    { forward = bool_int kb.kw -.. bool_int kb.ks; right = bool_int kb.kd -.. bool_int kb.ka; jump = kb.kspace }
  in
  let player = Minecraft_player.step world ~dt input player in
  let m = { m with player; block } in
  edit computer m;
  { m with last_time = Some now; was_down = computer.mouse.mdown; was_right_down = computer.mouse.mrdown; was_tab = tab }

let crosshair : shape3d =
  hud (Playground.group [ Playground.rectangle Playground.black 20. 2.; Playground.rectangle Playground.black 2. 20. ])

let status (computer : Playground.computer) (m : model) : shape3d =
  let (x, y, z) = m.player.position in
  let block = match m.block with Brick -> "brick" | Grass -> "grass" | Sand -> "sand" | Stone -> "stone" in
  hud
    (Playground.words Playground.black
       (Printf.sprintf "%s  (%.0f, %.0f, %.0f)%s" block x y z (if m.player.flying then "  flying" else ""))
    |> Playground.move (computer.screen.left + 150.) (computer.screen.top - 30.))

let view (computer : Playground.computer) (m : model) : camera * shape3d list =
  let (x, y, z) = m.player.position and (sx, sy, sz) = Minecraft_player.sight_vector m.player in
  (* the original's field of view, 65 degrees; far enough for the whole
   * world, which it cuts at 60 blocks behind fog instead *)
  let cam = camera ~eye:(x, y, z) ~target:(x + sx, y + sy, z + sz) ~fov:65. ~far:300. () in
  (cam, Hashtbl.fold (fun _ chunk l -> chunk :: l) chunks [] @ [ crosshair; status computer m ])

let app = game3d view update initial

(* claude: sharp texels, like the original's GL_NEAREST: bilinear
 * filtering blurs the pixel-art blocks, and blends each atlas cell with
 * its neighbors in the atlas along its borders *)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with smooth_textures = false } app
