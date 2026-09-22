(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Fez (Phil Fish, Polytron, 2012): a 2D platformer in
 * a 3D world, seen from one side at a time and turned a quarter at a
 * time.
 *
 *   left right   run         space   jump
 *   q e          turn the world a quarter, one way or the other
 *
 * Gomez lives in a flat world until a red fez shows him the third
 * dimension -- and then he can turn it, but still only see it flat.
 * Seen from the side, depth does not exist: two ledges far apart in
 * depth but side by side on the screen are side by side, and he walks
 * from one to the other. Five years of one designer's work, told in a
 * documentary (Indie Game: The Movie, 2012) that made it the emblem of
 * the indie game; and a pixel-art world in pastel colours, a tribute to
 * the 8-bit games whose look it took and whose limits it did not have.
 * (Names and dates from memory, to check.)
 *
 * It is the family of TinyCrush, where depth is flattened too, but
 * with Fez's rules, not Crush's (the model here is this game's own,
 * not gamekits/crush):
 *
 *   - the world is always flat -- there is no uncrushed view -- and
 *     turned four ways ([view]: front, right, back, left);
 *   - a floor is any block's top, at any depth: Gomez lands on it and
 *     is at its depth from then on ([landing]);
 *   - a wall is only a block at his depth or nearer: what is behind him
 *     is background, and he walks in front of it ([walls]) -- which is
 *     what makes Fez's world feel flat;
 *   - turning a quarter swaps across and depth; if that puts him behind
 *     something, he is brought in front of it ([to_front]), since what
 *     is seen is what is true.
 *
 * The picture, and the trick of this game ([faces]): the world is
 * drawn as the cubes' side faces in an orthographic projection turned
 * by the angle of the view. At a quarter turn exactly, only the faces
 * turned towards us show, full width: a flat picture. In between, a
 * cube seen from the side turning by an angle a shows two faces, of
 * widths |cos a| and |sin a| -- the pixel-art textures squashed to
 * those widths, one run of pixels after another ([face_shape]) -- and
 * painted far first:
 *
 *          a = 0          a = 30          a = 90
 *         +------+       +----+--+       +------+
 *         | front|       |front|si|       | side |
 *         +------+       +----+--+       +------+
 *                         cos 30  sin 30
 *
 * The look: Fez's pastel sky in bands, brick and grass as 8x8 pixel
 * textures, Gomez in white with his red fez, the golden cube bits --
 * all drawn in XPM files beside the game (fez_*.xpm, the format
 * TinyAseprite writes and GIMP opens), embedded at build time; the
 * farther a face, the more of the sky's colour it takes (fog), which
 * is the only depth a flat view shows.
 *
 * What it uses: gamekits/platformer's Tile_move (running and jumping,
 * on tile maps built each frame from the world and the view), Tilemap,
 * Sprite (the XPM files read, pixel art drawn, and the runs a squashed
 * face is drawn from), Scene2d. Not gamekits/crush: its rules are
 * Crush's, not these.
 *
 * Left undone, exercises: the doors to other levels and the map of
 * them, Fez's warp gates; the anti-cubes and their codes (Fez's second
 * game, a puzzle in a language to decipher); ladders, vines and the
 * blocks that crumble; turning in mid-air; the rotation of the camera
 * round Gomez rather than round the level's centre.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

(* n x n columns, h high; cells (x, y, z), y up, z away from the front *)
let n = 7
let h = 12
let tile = 48.

(* the level as boxes: (x0, x1, y0, y1, z0, z1), each inclusive *)
let boxes =
  [ (2, 4, 0, 1, 2, 4) (* the island *);
    (3, 3, 2, 7, 3, 3) (* the tower *);
    (5, 5, 2, 2, 3, 3) (* east, low *);
    (3, 3, 3, 3, 1, 1) (* south *);
    (1, 1, 4, 4, 3, 3) (* west *);
    (3, 3, 5, 5, 5, 5) (* north *);
    (* east, high, a row forward: at the tower's depth it would hang
     * right over the west ledge seen from the right, a ceiling *)
    (5, 5, 6, 6, 2, 2) ]

let solid_cell : bool array =
  let a = Array.make (n *.. h *.. n) false in
  List.iter
    (fun (x0, x1, y0, y1, z0, z1) ->
      for x = x0 to x1 do for y = y0 to y1 do for z = z0 to z1 do a.((((x *.. h) +.. y) *.. n) +.. z) <- true done done done)
    boxes;
  a

let block (x : int) (y : int) (z : int) : bool =
  x >= 0 && y >= 0 && z >= 0 && x < n && y < h && z < n && solid_cell.((((x *.. h) +.. y) *.. n) +.. z)

(* the cube bits, on top of the ledges and the tower *)
let all_bits = [ (3, 4, 1); (3, 6, 5); (3, 8, 3) ]

let start_pos = (2, 2, 2) (* on the island, its front-left corner *)

(*****************************************************************************)
(* The views: four ways to flatten the world *)
(*****************************************************************************)

(* [to_grid view u d]: the column (x, z) that is at [u] across and [d]
 * deep in [view]; [of_grid] the other way. View 0 is the front (x
 * across, z deep); each next view is the world turned a quarter. *)
let to_grid (view : int) (u : int) (d : int) : int * int =
  match view land 3 with 0 -> (u, d) | 1 -> (n -.. 1 -.. d, u) | 2 -> (n -.. 1 -.. u, n -.. 1 -.. d) | _ -> (d, n -.. 1 -.. u)

let of_grid (view : int) (x : int) (z : int) : int * int =
  match view land 3 with 0 -> (x, z) | 1 -> (z, n -.. 1 -.. x) | 2 -> (n -.. 1 -.. x, n -.. 1 -.. z) | _ -> (n -.. 1 -.. z, x)

(* the depths along the line of sight at [u], height [y], that hold a
 * block *)
let depths (view : int) (u : int) (y : int) : int list =
  List.filter (fun d -> let x, z = to_grid view u d in block x y z) (List.init n Fun.id)

(* a tile map of the flat world, a cell solid if [keep] its depths: rows
 * from the top, as Tilemap types them *)
let flat (view : int) (keep : int -> int list -> bool) : Tilemap.t =
  Tilemap.of_strings tile
    (List.init h (fun r ->
         let y = h -.. 1 -.. r in
         String.init n (fun u -> if keep y (depths view u y) then '#' else '.')))

(* the walls: blocks at Gomez's depth or nearer; what is behind is
 * background *)
let walls (view : int) (depth : int) : Tilemap.t = flat view (fun _ ds -> List.exists (fun d -> d <= depth) ds)

(* the floors, for a fall from [feet]: any block whose top is not above
 * the feet -- whatever its depth *)
let floors (view : int) (feet : number) : Tilemap.t =
  flat view (fun y ds -> ds <> [] && ((float_of_int (y +.. 1) - (float_of_int h / 2.)) * tile) <= feet + 0.5)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  view : int;
  turning : (int * number) option; (* which way (1 or -1), and how far (0 to 1) *)
  u : number; (* across and up, in the flat world's coordinates *)
  y : number;
  depth : int; (* for walls: the depth Gomez is at *)
  on_depth : int; (* the depth of the block he last stood on *)
  vy : number;
  ground : bool;
  facing : number;
  bits : (int * int * int) list; (* still to find *)
  safe : int * number * number * int; (* where he last stood: view, u, y, depth *)
  frames : int;
}

type scene = Title | Playing of play | Whole
type model = scene Scene2d.t

let size = (26., 40.)

(* the centre of a cell, in the flat world's coordinates *)
let center_u (u : int) : number = (float_of_int u - (float_of_int (n -.. 1) / 2.)) * tile
let center_y (y : int) : number = (float_of_int y - (float_of_int (h -.. 1) / 2.)) * tile

let start () : play =
  let x, y, z = start_pos in
  let u, d = of_grid 0 x z in
  let gy = center_y y - (tile / 2.) + (snd size / 2.) in
  { view = 0; turning = None; u = center_u u; y = gy; depth = d; on_depth = d; vy = 0.; ground = true; facing = 1.;
    bits = all_bits; safe = (0, center_u u, gy, d); frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let cell_u (p : play) : int = int_of_float (Float.round ((p.u / tile) + (float_of_int (n -.. 1) / 2.)))
let feet (p : play) : number = p.y - (snd size / 2.)
let wall_c (c : char) : bool = c = '#'

(* in front of whatever would hide Gomez: his depth brought forward to
 * just before the nearest block his box overlaps *)
let to_front (p : play) : play =
  let map = walls p.view p.depth in
  if not (Tile_move.hits wall_c map size p.u p.y) then p
  else
    let near =
      List.concat_map
        (fun (du, dy) ->
          let u, r = Tilemap.cell map (p.u + du) (p.y + dy) in
          if u < 0 || u >= n then [] else depths p.view u (h -.. 1 -.. r))
        [ (-10., -15.); (10., -15.); (-10., 15.); (10., 15.); (0., 0.) ]
    in
    match near with [] -> p | ds -> { p with depth = List.fold_left min (List.hd ds) ds -.. 1 }

(* landing: at the depth of the nearest block under his feet *)
let landing (p : play) : play =
  let u = cell_u p in
  let y = int_of_float (Float.round ((feet p / tile) + (float_of_int h / 2.))) -.. 1 in
  match depths p.view u y with
  | [] -> p
  | ds ->
      let d = List.fold_left min (List.hd ds) ds in
      let p' = { p with depth = d; on_depth = d } in
      if Tile_move.hits wall_c (walls p.view d) size p.u p.y then { p' with depth = p.depth } else p'

(* the turn done: across and depth swapped; Gomez where his block
 * puts him in the new view, then in front of what would hide him *)
let turned (dir : int) (p : play) : play =
  let x, z = to_grid p.view (cell_u p) p.on_depth in
  let view = (p.view +.. dir +.. 4) land 3 in
  let u, d = of_grid view x z in
  to_front { p with view; turning = None; u = center_u u; depth = d; on_depth = d }

let gravity = 0.8
let jump_speed = 11.
let run_speed = 4.

(* what the player does this frame *)
type input = { dx : number; jump : bool; turn : int }

let nothing = { dx = 0.; jump = false; turn = 0 }

(* the bits Gomez's box touches, in the flat world: at any depth *)
let take_bits (p : play) : play =
  let near (x, y, z) =
    let u, _ = of_grid p.view x z in
    Float.abs (center_u u - p.u) < 30. && Float.abs (center_y y - p.y) < 34.
  in
  { p with bits = List.filter (fun b -> not (near b)) p.bits }

let step (i : input) (p : play) : play =
  let p = { p with frames = p.frames +.. 1 } in
  match p.turning with
  | Some (dir, t) -> if t >= 1. then turned dir p else { p with turning = Some (dir, t + (1. / 16.)) }
  | None when i.turn <> 0 && p.ground -> { p with turning = Some (i.turn, 0.) }
  | None ->
      let vy = if i.jump && p.ground then jump_speed else Float.max (-14.) (p.vy - gravity) in
      (* across, against the walls; up against them too; down onto any floor *)
      let (u, y), _ = Tile_move.move_by wall_c (walls p.view p.depth) size (p.u, p.y) (i.dx * run_speed, 0.) in
      let ymap = if vy > 0. then walls p.view p.depth else floors p.view (feet p) in
      let (u, y), hit_y = Tile_move.move_by wall_c ymap size (u, y) (0., vy) in
      let p = { p with u; y; vy = (if hit_y then 0. else vy); facing = (if i.dx <> 0. then i.dx else p.facing) } in
      let ground = Tile_move.on_ground wall_c (floors p.view (feet p)) size p.u p.y in
      let p = { p with ground } in
      let p = if ground && p.vy <= 0. then landing p else p in
      let p = if ground then { p with safe = (p.view, p.u, p.y, p.depth) } else p in
      take_bits p

(* fallen off the world: back where he last stood *)
let fell (p : play) : bool = p.y < -.(float_of_int h * tile / 2.) - 60.

let respawn (p : play) : play =
  let view, u, y, d = p.safe in
  { p with view; u; y; depth = d; on_depth = d; vy = 0.; ground = true; turning = None }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  match scenes.scene with
  | Title | Whole -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (start ())) scenes else scenes
  | Playing p ->
      let k = computer.keyboard in
      let i =
        { dx = to_x k; jump = pressed (fun k -> k.kspace);
          turn = (if pressed (fun k -> Set_.mem "e" k.keys) then 1 else if pressed (fun k -> Set_.mem "q" k.keys) then -1 else 0) }
      in
      let p = step i p in
      let p = if fell p then respawn p else p in
      if p.bits = [] then Scene2d.go Whole scenes else { scenes with scene = Playing p }

(*****************************************************************************)
(* View: pixel art, turned *)
(*****************************************************************************)

type rgb3 = int * int * int

let sky_top : rgb3 = (112, 180, 232)
let sky_low : rgb3 = (250, 206, 214)

let mix ((r1, g1, b1) : rgb3) ((r2, g2, b2) : rgb3) (t : number) : rgb3 =
  let m a b = int_of_float (float_of_int a + ((float_of_int b - float_of_int a) * t)) in
  (m r1 r2, m g1 g2, m b1 b2)

let color ((r, g, b) : rgb3) : color = rgb r g b

(* The pixel art, drawn in XPM files beside the game (fez_*.xpm, a
 * sprite editor's format, TinyAseprite's) and embedded by dune
 * as Fez_xpm, so that the game needs no file at run time. *)

let rgb3_of (c : color) : rgb3 =
  match c with
  | Color.Rgb (r, g, b) -> (r, g, b)
  | Color.Hex hex -> (int_of_string ("0x" ^ String.sub hex 1 2), int_of_string ("0x" ^ String.sub hex 3 2), int_of_string ("0x" ^ String.sub hex 5 2))

let xpm (text : string) : (char * rgb3) list * string list =
  let palette, rows = Sprite.of_xpm text in
  (List.map (fun (ch, c) -> (ch, rgb3_of c)) palette, rows)

(* the faces, 8x8: brick, and brick with grass on top *)
let brick_palette, brick = xpm Fez_xpm.brick
let grass_palette, grassy = xpm Fez_xpm.grass

(* the stone a shade darker for each direction a face looks, so that
 * the turn shows which is which; the grass as drawn *)
let stone (normal : int) (grass : bool) : (char * rgb3) list =
  let shade = float_of_int normal * 0.07 in
  List.map
    (fun (ch, col) -> if ch = 'g' || ch = 'G' then (ch, col) else (ch, mix col (40, 20, 60) shade))
    (if grass then grass_palette else brick_palette)

(* [face_shape tex palette w]: an 8x8 texture drawn [w] wide and a tile
 * high, each run of one colour one rectangle, squashed across -- a
 * face turned away by an angle *)
let face_shape (tex : string list) (palette : (char * rgb3) list) (w : number) : shape =
  let px = tile / 8. and sx = w / 8. in
  group
    (List.concat
       (List.mapi
          (fun r row ->
            List.filter_map
              (fun (start, len, c) ->
                match List.assoc_opt c palette with
                | None -> None
                | Some col ->
                    Some
                      (rectangle (color col) ((float_of_int len * sx) + 0.6) (px + 0.6)
                      |> move ((float_of_int start * sx) + (float_of_int len * sx / 2.) - (w / 2.)) ((tile / 2.) - (float_of_int r * px) - (px / 2.))))
              (Sprite.runs row))
          tex))

(* the four side faces of a cube: their outward normal, 0 front (-z),
 * 1 right (+x), 2 back (+z), 3 left (-x) *)
let normals = [ (0, (0, -1)); (1, (1, 0)); (2, (0, 1)); (3, (-1, 0)) ]

(* the trick of this game, in 26 lines (see the header): the faces a
 * turn by [angle] shows, each an (x across, depth, width) -- the side
 * faces' centres turned, their widths the cosine or the sine -- far
 * first *)
let faces (angle : number) : (number * number * number * int * int * int * int) list =
  let c = Float.cos (angle * Float.pi / 180.) and s = Float.sin (angle * Float.pi / 180.) in
  let mid = float_of_int (n -.. 1) / 2. in
  let found = ref [] in
  for x = 0 to n -.. 1 do
    for y = 0 to h -.. 1 do
      for z = 0 to n -.. 1 do
        if block x y z then
          List.iter
            (fun (k, (nx, nz)) ->
              let fx = float_of_int x - mid + (float_of_int nx / 2.) and fz = float_of_int z - mid + (float_of_int nz / 2.) in
              let toward = (float_of_int nz * c) - (float_of_int nx * s) in
              if toward < -0.001 && not (block (x +.. nx) y (z +.. nz)) then
                let across = (fx * c) + (fz * s) and depth = (fz * c) - (fx * s) in
                let width = Float.abs ((float_of_int nx * s) - (float_of_int nz * c)) in
                found := (across * tile, depth, width * tile, x, y, z, k) :: !found)
            normals
      done
    done
  done;
  List.sort (fun (_, d1, _, _, _, _, _) (_, d2, _, _, _, _, _) -> compare d2 d1) !found

let gomez_palette, gomez_stand = Sprite.of_xpm Fez_xpm.gomez_stand
let gomez_walk = snd (Sprite.of_xpm Fez_xpm.gomez_walk)

let gomez_shape (p : play) : shape =
  let rows = if p.ground && p.frames /.. 8 mod 2 = 0 then gomez_stand else gomez_walk in
  let rows = if p.facing < 0. then Sprite.flip rows else rows in
  Sprite.pixels 4.5 gomez_palette rows

let bit_palette, bit_rows = Sprite.of_xpm Fez_xpm.bit

let bit_shape (frames : int) : shape =
  (* the glint: the light yellow brighter every other beat *)
  let palette = if frames /.. 10 mod 2 = 0 then bit_palette else List.map (fun (ch, c) -> if ch = 'Y' then (ch, rgb 250 214 90) else (ch, c)) bit_palette in
  Sprite.pixels 4. palette bit_rows

let cloud = Sprite.pixels 6. [ ('w', rgb 255 255 255) ] [ "...ww....."; ".wwwwww..."; "wwwwwwwwww"; ".wwwwwwww." ]

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  (* the angle of the view, turning or not *)
  let angle = (float_of_int p.view * 90.) + match p.turning with Some (dir, t) -> float_of_int dir * 90. * t | None -> 0. in
  let sky =
    List.init 12 (fun i ->
        let t = float_of_int i / 11. in
        rectangle (color (mix sky_top sky_low t)) screen.width ((screen.height / 12.) + 2.)
        |> move_y (screen.top - ((float_of_int i + 0.5) * screen.height / 12.)))
  in
  let clouds = List.map (fun (x, y) -> cloud |> move (Float.rem (x + (float_of_int p.frames * 0.3)) 1400. - 700.) y) [ (0., 280.); (500., 200.); (900., 330.) ] in
  let mid = float_of_int (n -.. 1) / 2. in
  (* things that are not cubes, drawn among the faces by their depth:
   * the bits, and Gomez *)
  let turn_point (x : number) (z : number) =
    let c = Float.cos (angle * Float.pi / 180.) and s = Float.sin (angle * Float.pi / 180.) in
    let fx = x - mid and fz = z - mid in
    (((fx * c) + (fz * s)) * tile, (fz * c) - (fx * s))
  in
  let bits =
    List.map
      (fun (x, y, z) ->
        let sx, d = turn_point (float_of_int x) (float_of_int z) in
        (d, bit_shape p.frames |> move sx (center_y y + (Float.sin (float_of_int p.frames * 0.1) * 4.))))
      p.bits
  in
  let gomez =
    match p.turning with
    | None -> (float_of_int p.depth - 0.6 - mid, gomez_shape p |> move p.u p.y)
    | Some _ ->
        let gx, gz = to_grid p.view (cell_u p) p.on_depth in
        let sx, d = turn_point (float_of_int gx) (float_of_int gz) in
        (d - 0.6, gomez_shape p |> move sx p.y)
  in
  let cubes =
    List.map
      (fun (sx, d, w, x, y, z, k) ->
        let grass = not (block x (y +.. 1) z) in
        let tex = if grass then grassy else brick in
        (* fog: the farther, the more of the sky *)
        let fog = Float.max 0. ((d + mid) / float_of_int n * 0.45) in
        let palette = List.map (fun (ch, col) -> (ch, mix col (mix sky_top sky_low 0.5) fog)) (stone k grass) in
        (d, face_shape tex palette w |> move sx (center_y y)))
      (faces angle)
  in
  (* the painter's order: far first *)
  let things = List.sort (fun (d1, _) (d2, _) -> compare d2 d1) (cubes @ bits @ [ gomez ]) in
  let found = List.length all_bits -.. List.length p.bits in
  (* the world drawn a third bigger than it is played, for the pixels
   * to show *)
  sky @ clouds @ [ group (List.map snd things) |> scale 1.35 |> move_y 30. ]
  @ [ words white (Printf.sprintf "cube bits %d / %d" found (List.length all_bits)) |> scale 2. |> move (screen.left + 150.) (screen.top - 40.);
      words (rgb 90 60 120) "arrows run   space jump   q e turn the world" |> scale 1.6 |> move_y (screen.bottom + 30.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let bg = rectangle (color sky_top) screen.width screen.height in
  match model.scene with
  | Title ->
      [ bg; words white "TINY FEZ" |> scale 7. |> move_y 200.;
        words white "the world is flat, and you can turn it" |> scale 2. |> move_y 60.;
        words white "what lines up on the screen is next to each other" |> scale 2. |> move_y 25.;
        gomez_shape (start ()) |> scale 2. |> move_y (-80.) ]
      @ Scene2d.blink 1. model [ words white "PRESS SPACE" |> scale 3. |> move_y (-220.) ]
  | Playing p -> view_play computer p
  | Whole -> [ bg; bit_shape 0 |> scale 4. |> move_y 80.; words white "THE CUBE IS WHOLE" |> scale 4. |> move_y (-40.) ]

let help = {|TinyFez
  left right run, space jump, q e turn the world a quarter
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
