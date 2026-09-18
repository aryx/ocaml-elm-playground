(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Phase 2 of docs/claude_notes/plan_tiny_minecraft.md: static
 * rendering of Minecraft_model.ml's generated world -- a fixed
 * overview camera, no controls yet (first-person movement is Phase
 * 3). Rendering ~50k exposed blocks rebuilt from scratch every frame
 * (this project's usual game3d style) measured at ~0.2 fps; the world
 * is now built once, as cached chunks, see [chunks] below and
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
 * rather than one cached3d for the whole world so that an edit (not
 * yet possible) only rebuilds the chunks it touches. *)
let chunks (m : Minecraft_model.t) : shape3d list =
  Hashtbl.fold
    (fun _sector positions acc ->
      let blocks =
        !positions
        |> List.filter_map (fun pos ->
               Hashtbl.find_opt m.shown pos |> Option.map (fun block -> block_shape m pos block))
      in
      cached3d blocks :: acc)
    m.sectors []

let world = Minecraft_model.create_world ()
let world_chunks = chunks world

let view (_computer : Playground.computer) () : camera * shape3d list =
  let cam = camera ~eye:(0., 40., 60.) ~target:(0., 0., 0.) ~far:400. () in
  (cam, world_chunks)

let update _computer () = ()
let app = game3d view update ()

(* claude: sharp texels, like the original's GL_NEAREST: bilinear
 * filtering blurs the pixel-art blocks, and blends each atlas cell with
 * its neighbors in the atlas along its borders *)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with smooth_textures = false } app
