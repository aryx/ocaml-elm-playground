(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of The Legend of Zelda: A Link to the Past (Nintendo,
 * 1991, on the Super Nintendo): Hyrule seen from above, a lake and its
 * island, a plateau up a stair, the Lost Woods, and the Master Sword
 * in its pedestal, which only the three pendants of virtue will free.
 * Arrows to walk (diagonals too), space to swing the sword -- at the
 * soldiers, at the bushes, and at the pedestal once you have the three.
 *
 * TinyZelda is the first Zelda, on the NES, drawn straight from above,
 * a room at a time; this one is the third, five years later, on a
 * console with more colours and more sprites, and it looks different
 * for one reason: its world is seen from above *and from the front*,
 * the view of the Japanese RPGs of the time, and of most 2D action
 * games since (Stardew Valley, Undertale). The ground is a map, seen
 * from straight above; everything standing on it -- Link, a soldier, a
 * tree, the face of a cliff -- is seen from the front, as tall as it is,
 * rising up the screen from where it touches the ground. The scrolling
 * came with it: no more rooms here, the camera follows Link across the
 * whole map. (Names and dates from memory, to check.)
 *
 * The trick of this game, the three-quarter view ([standing],
 * [draw_standing]):
 *
 *        screen                           the world, from the side
 *     +----------+
 *     |  (tree)  |  the canopy, drawn          canopy
 *     |  (    )  |  up the screen, over        ( ~~ )
 *     |  ( @  )  |  cells *behind* the       (  ~~  )   @ Link,
 *     |    ||    |  trunk: Link there         ( ~~ )    behind it
 *     |    ||    |  is hidden                   ||     /
 *     +----T-----+  <- where it stands,        _||____@______ ground
 *                      its base line
 *
 * Nothing in the model has a height: the world is a flat tile map, and
 * the collisions are TinyZelda's, boxes against solid cells -- a tree
 * is its trunk's one cell, a cliff a row of solid cells, a character
 * his feet. The height is only in the pictures: each thing standing on
 * the ground is drawn with its bottom on its base line (its feet, the
 * foot of its trunk) and rises from there over what is behind it. So
 * the order of drawing is the whole trick: the ground first, flat, and
 * then the standing things, farthest first -- the higher up the screen
 * their base line, the farther -- a painter's algorithm with one number
 * per thing as its key. Walk north of a tree and you go behind its
 * canopy; walk south of it and you are in front. The same sort puts
 * Link behind a soldier or in front of him, and the sword behind Link
 * when he strikes north.
 *
 * Why no ambiguity, when Zaxxon's sort (TinyZaxxon, the isometric kit)
 * has some: every standing thing here is a flat picture facing the
 * camera, and two pictures facing the camera are ordered by their
 * distance from it alone -- the base line. A long thing lying *along*
 * the depth (a wall going north) would not be; this game has none, and
 * the cliffs are drawn in the ground, flat, their height painted in
 * their tiles (the grass lip, the rock face): the art is the only place
 * where they are high.
 *
 * The pixel art is in XPM files beside the game (lttp_*.xpm, TinyFez's
 * and TinyMario's format, the one TinyAseprite edits and GIMP opens),
 * embedded by dune as Lttp_xpm. Link's six poses are one sheet, 96 x
 * 22, cut in 16-pixel frames here ([frames]): the sprite sheet of every
 * console game since the NES. The ground's green is not in the tiles:
 * it is one big rectangle under the map, and the tiles only draw their
 * blades of grass on it, on one cell in four; the other tiles are
 * painted from their main colour ([on_base]); and every picture is
 * drawn in the fewest rectangles its palette's order allows ([paint]).
 *
 * The rest of the depth is painted too, as it was on a console with no
 * 3D at all. The light always comes from the top left: every round
 * thing is lighter up and left, darker down and right, and throws its
 * shadow down and right ([cast_shadow]: the map drawn a second time,
 * a shadow per tile). A cliff is a plateau's grass top, a lit lip, and
 * a face of boulders darker toward its foot ('^' then '#'), its west
 * side a narrow wall ('|'); the lake's north shore shows the bank's
 * earth falling into the water ('w'). And Link and the soldiers carry
 * their own shadow, a small oval drawn apart from them ([shadow]), as
 * the original's sprites did -- the shadow that stays on the ground
 * when Link jumps down a ledge.
 *
 * What it uses: the platformer kit's Tile_move (gamekits/platformer/:
 * Link and the soldiers are boxes against the map's solid cells, one
 * pixel at a time, as in TinyZelda), Tilemap (the world, changed by the
 * bushes cut and the things taken), Camera2d (look_at and clamp:
 * scrolling), Sprite (of_xpm, flip), Scene2d. Not Physics:
 * nothing slides, but the knockback. Not the isometric kit: its two
 * lines turn three world axes into two screen ones, where here the
 * ground is only the screen moved (x across, y up the screen), and the
 * height is in the art.
 *
 * Exercises: lifting a bush or a pot and throwing it (the carried thing
 * drawn above Link's head, standing on his base line); the spin attack,
 * the sword held and released; the Light World and the Dark World, the
 * same map twice and a mirror between them (the original's big idea);
 * a house to enter, the scroll replaced by a door; a real second level,
 * a bridge you walk under -- the one thing a single flat map can't do,
 * which the original did with two layers of tiles and a bit per
 * character saying which one he is on.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

(* the art's pixels drawn 3 x 3, so a 16-pixel tile is 48 wide *)
let px = 3.
let tile = 16. * px

(* 40 x 30 cells: ' ' grass, ',' flowers, '.' a path, '~' water, '^'
 * the lip of a cliff and '#' its face, '|' its side, '=' stairs, 'H' a
 * bridge (and 'w', the lake's north shore, found by [start_map]); '*' a
 * bush, 'o' a rock; T a tree (its trunk: the canopy stands over the
 * cells above), M the Master Sword; 1 2 3 the pendants; L Link, s the
 * soldiers *)
let world_rows =
  [ "                                        ";
    "T T T T T T T TT        T T T T T T T T ";
    "                   M                    ";
    " T T T T T T T T        TT T T T T T T T";
    "        1        ,,,,,,                 ";
    "T T T T T T T TT        T T T T T T T T ";
    "                   ..                   ";
    " T T T T T T T T T ..  T T T T T T T T T";
    "                   ..                   ";
    "T T T T T T T T T  .. T T T T T T T T T ";
    "                   ..                   ";
    "T                  ..    *  |       *  T";
    "         o          .       |    o      ";
    "T    *    s         .   s   | *    2   T";
    "            *    ** .  *    |           ";
    "T   ,,     *        .   *   |     s  o T";
    "   *,,              .     o |           ";
    "T             o     .       ^^^=^^^^^^^T";
    "                    . *     ###=####### ";
    "T ~~~~~~~~~~~~~ *   .       ###=####### ";
    "  ~~~~~~~~~~~~~  s  .  ,,,              ";
    "T ~~~~~~~~~~~~~     .  ,,,    s         ";
    "  ~~~~   *~~~~~*    .                   ";
    "T ~~~~ 3  HHHHH*.....            o      ";
    "  ~~~~,   ~~~~~*    .         ,,,       ";
    "T ~~~~~~~~~~~~~     .     **  ,,,  o    ";
    "  ~~~~~~~~~~~~~     L               s   ";
    "T ~~~~~~~~~~~~~         o               ";
    "                                        ";
    "T T T T T T T T T T T T T T T T T T T T " ]

let level = Tilemap.of_strings tile world_rows
let bounds = Tilemap.bounds level
let solid (c : char) : bool = match c with '~' | 'w' | '^' | '#' | '|' | '*' | 'o' | 'T' | 'M' -> true | _ -> false

(* the boxes that collide: feet, not bodies *)
let feet = (36., 24.)

let places (c : char) : (number * number) list = List.map (fun (col, row) -> Tilemap.center level col row) (Tilemap.find level c)

(* the world to play: Link and the soldiers are not tiles; the grass is
 * 'g', plain, or 'v', a cell in four scattered, with blades; and the
 * water with land above it is 'w', the north shore, where the bank's
 * earth face shows, falling into the lake *)
let start_map : Tilemap.t =
  let grass =
    List.fold_left
      (fun m (c, r) -> Tilemap.set m c r (if ((c *.. 7) +.. (r *.. 5)) mod 4 = 0 then 'v' else 'g'))
      level
      (List.concat_map (Tilemap.find level) [ ' '; 'L'; 's' ])
  in
  List.fold_left
    (fun m (c, r) -> match Tilemap.get level c (r -.. 1) with Some ('~' | 'H') -> m | _ -> Tilemap.set m c r 'w')
    grass (Tilemap.find level '~')

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type soldier = {
  sx : number; (* the center of his feet *)
  sy : number;
  dir : number * number;
  turn : int; (* frames before he picks another way, when wandering *)
  hp : int;
  knock : int; (* frames of being thrown back, after a blow *)
  away : number * number;
}

type game = {
  map : Tilemap.t;
  x : number; (* Link: the center of his feet *)
  y : number;
  facing : number * number; (* one of the four ways *)
  steps : int; (* frames walked, for his legs *)
  swing : int; (* frames of the sword's swing left *)
  hurt : int; (* frames of blinking left, after a hit *)
  push : number * number; (* the knockback *)
  life : int; (* in half hearts *)
  max_life : int;
  rupees : int;
  pendants : char list;
  soldiers : soldier list;
  leaves : (number * number * int) list; (* bushes cut: where, and since when *)
  seed : int;
  cam : Camera2d.t;
  frames : int;
}

type scene = Title | Playing of game | Won of int | Game_over
type model = scene Scene2d.t

let screen0 = to_screen default_width default_height

(* the camera on Link, but never showing past the map's edges -- the
 * top one a band higher, for the map's first row not to be under the
 * line of hearts and rupees *)
let camera (x : number) (y : number) (cam : Camera2d.t) : Camera2d.t =
  Camera2d.look_at x (y + 30.) cam |> Camera2d.clamp screen0 { bounds with top = bounds.top + 100. }

let new_game () : game =
  let x, y = List.hd (places 'L') in
  let soldier (sx, sy) = { sx; sy; dir = (0., -1.); turn = 0; hp = 2; knock = 0; away = (0., 0.) } in
  { map = start_map; x; y; facing = (0., 1.); steps = 0; swing = 0; hurt = 0; push = (0., 0.); life = 6; max_life = 6; rupees = 0;
    pendants = []; soldiers = List.map soldier (places 's'); leaves = []; seed = 7; cam = camera x y Camera2d.origin; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* TinyZelda's linear congruential generator *)
let next (seed : int) : int = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff

let dirs4 = [| (0., 1.); (0., -1.); (1., 0.); (-1., 0.) |]

(* the point the sword reaches, in front of Link's body *)
let reach (g : game) : number * number = (g.x + (fst g.facing * 44.), g.y + 20. + (snd g.facing * 44.))

(* a soldier wanders, until he sees Link: then he comes at him *)
let march (g : game) (s : soldier) : game * soldier =
  if s.knock > 0 then
    let (sx, sy), _ = Tile_move.move_by solid g.map feet (s.sx, s.sy) s.away in
    (g, { s with sx; sy; knock = s.knock -.. 1 })
  else
    let dx = g.x - s.sx and dy = g.y - s.sy in
    let d = Float.max 1. (Float.hypot dx dy) in
    let g, s =
      if d < 260. then (g, { s with dir = (dx / d, dy / d) })
      else if s.turn <= 0 then
        let seed = next g.seed in
        ({ g with seed }, { s with dir = dirs4.(seed mod 4); turn = 40 +.. (seed /.. 8 mod 80) })
      else (g, { s with turn = s.turn -.. 1 })
    in
    let speed = if d < 260. then 1.8 else 1.1 in
    let (sx, sy), hit = Tile_move.move_by solid g.map feet (s.sx, s.sy) (speed * fst s.dir, speed * snd s.dir) in
    (g, { s with sx; sy; turn = (if hit then 0 else s.turn) })

let hurt_by (sx : number) (sy : number) (g : game) : game =
  if g.hurt > 0 then g
  else begin
    Audio.play Audio.hit;
    let d = Float.max 1. (Float.hypot (g.x - sx) (g.y - sy)) in
    { g with life = g.life -.. 1; hurt = 60; push = (7. * (g.x - sx) / d, 7. * (g.y - sy) / d) }
  end

(* what a bush hides: mostly nothing, sometimes a rupee, a heart *)
let hidden (seed : int) : char = match seed /.. 16 mod 6 with 0 | 1 -> 'r' | 2 -> 'h' | _ -> 'g'

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  let k = computer.keyboard in
  (* Link: thrown back after a hit, swinging (standing still), or
   * walking, in eight directions; going diagonally, he keeps facing
   * the way across if he was, else faces the way up or down *)
  let dx = to_x k and dy = to_y k in
  let g =
    if g.hurt > 48 then
      let (x, y), _ = Tile_move.move_by solid g.map feet (g.x, g.y) g.push in
      { g with x; y }
    else if g.swing > 0 then { g with swing = g.swing -.. 1 }
    else if Scene2d.pressed (fun k -> k.kspace) scenes then (Audio.play Audio.laser; { g with swing = 16 })
    else if dx <> 0. || dy <> 0. then
      let n = if dx <> 0. && dy <> 0. then 0.7 else 1. in
      let (x, y), _ = Tile_move.move_by solid g.map feet (g.x, g.y) (3.5 * n * dx, 3.5 * n * dy) in
      let facing =
        if dy = 0. then (dx, 0.) else if dx = 0. then (0., dy) else if g.facing = (dx, 0.) then g.facing else (0., dy)
      in
      { g with x; y; facing; steps = g.steps +.. 1 }
    else g
  in
  let g = { g with hurt = max 0 (g.hurt -.. 1) } in
  (* the map's own limits, where there is no tree to stop him *)
  let g =
    { g with x = Float.max (bounds.left + 20.) (Float.min (bounds.right - 20.) g.x); y = Float.max (bounds.bottom + 12.) (Float.min (bounds.top - 60.) g.y) }
  in
  (* what he walks on *)
  let col, row = Tilemap.cell g.map g.x g.y in
  let take g = { g with map = Tilemap.set g.map col row 'g' } in
  let g =
    match Tilemap.get g.map col row with
    | Some ('1' | '2' | '3' as p) -> Audio.play Audio.coin; take { g with pendants = p :: g.pendants }
    | Some 'r' -> Audio.play Audio.coin; take { g with rupees = g.rupees +.. 5 }
    | Some 'h' -> Audio.play Audio.coin; take { g with life = min g.max_life (g.life +.. 2) }
    | _ -> g
  in
  (* the sword, at the middle of its swing: a bush in front is cut *)
  let rx, ry = reach g in
  let g =
    if g.swing <> 10 then g
    else
      let c, r = Tilemap.cell g.map rx (ry - 20.) in
      if Tilemap.get g.map c r = Some '*' then begin
        Audio.play Audio.step;
        let seed = next g.seed in
        let cx, cy = Tilemap.center g.map c r in
        { g with map = Tilemap.set g.map c r (hidden seed); seed; leaves = (cx, cy, g.frames) :: g.leaves }
      end
      else g
  in
  let g = { g with leaves = List.filter (fun (_, _, t) -> g.frames -.. t < 24) g.leaves } in
  (* the soldiers: the ones near the screen march; the sword throws them
   * back, the second blow kills, and leaves a rupee *)
  let g, soldiers =
    List.fold_left
      (fun (g, acc) s -> if Float.abs (s.sx - g.x) > 700. || Float.abs (s.sy - g.y) > 700. then (g, s :: acc) else let g, s = march g s in (g, s :: acc))
      (g, []) g.soldiers
  in
  let struck s = g.swing > 3 && g.swing < 14 && s.knock = 0 && Float.hypot (s.sx - rx) (s.sy + 20. - ry) < 40. in
  let soldiers =
    List.map
      (fun s ->
        if not (struck s) then s
        else begin
          Audio.play Audio.hit;
          let d = Float.max 1. (Float.hypot (s.sx - g.x) (s.sy - g.y)) in
          { s with hp = s.hp -.. 1; knock = 12; away = (8. * (s.sx - g.x) / d, 8. * (s.sy - g.y) / d) }
        end)
      (List.rev soldiers)
  in
  let dead, soldiers = List.partition (fun s -> s.hp <= 0 && s.knock = 0) soldiers in
  let g =
    List.fold_left
      (fun g s ->
        Audio.play Audio.explosion;
        let c, r = Tilemap.cell g.map s.sx s.sy in
        match Tilemap.get g.map c r with Some ('g' | 'v') -> { g with map = Tilemap.set g.map c r 'r' } | _ -> g)
      { g with soldiers } dead
  in
  match List.find_opt (fun s -> s.knock = 0 && Float.hypot (s.sx - g.x) (s.sy - g.y) < 34.) g.soldiers with
  | Some s -> hurt_by s.sx s.sy g
  | None -> g

(* Link just below the pedestal, facing it *)
let at_pedestal (g : game) : bool =
  let c, r = Tilemap.cell g.map g.x (g.y + 40.) in
  g.facing = (0., 1.) && Tilemap.get g.map c r = Some 'M'

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      let g = { g with cam = camera g.x g.y g.cam } in
      if space && at_pedestal g && List.length g.pendants = 3 then (Audio.play Audio.coin; Scene2d.go (Won g.frames) s)
      else if g.life <= 0 then Scene2d.go Game_over s
      else { s with scene = Playing g }
  | Won _ | Game_over -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* The pixel art *)
(*****************************************************************************)

(* [frames w rows]: a sprite sheet cut in frames [w] pixels wide *)
let frames (w : int) (rows : string list) : string list array =
  Array.init (String.length (List.hd rows) /.. w) (fun i -> List.map (fun r -> String.sub r (i *.. w) w) rows)

(* [paint palette rows]: the picture in rectangles, laid down as a
 * painter would: the colours one after the other, in the palette's
 * order, each in spans that may run over the pixels of the colours
 * still to come (they will be painted over), never over those already
 * painted nor the transparent ones; and a span the same as the one
 * above it only makes that rectangle taller. The painter's algorithm of
 * [draw_standing], inside one picture. E.g. the tree's dark outline is
 * first one span a row, the whole canopy, the greens over it: 128
 * rectangles, where Sprite.pixels' runs are 244 (Sprite.mli's greedy
 * meshing, cheaply). Any order draws the same picture, some with fewer
 * rectangles: the XPM files list their colours in a good one. *)
let paint (palette : (char * color) list) (rows : string list) : shape =
  let rows = Array.of_list rows in
  let h = Array.length rows and w = String.length rows.(0) in
  let rec layers (palette : (char * color) list) : shape list =
    match palette with
    | [] -> []
    | (c, color) :: later ->
        let paintable ch = ch = c || List.mem_assoc ch later in
        (* row y's spans over [c] and the later colours, holding some [c] *)
        let spans y =
          let r = rows.(y) in
          let rec go i acc =
            if i >= w then acc
            else if not (paintable r.[i]) then go (i +.. 1) acc
            else
              let j = ref i in
              while !j < w && paintable r.[!j] do incr j done;
              go !j (if String.contains (String.sub r i (!j -.. i)) c then (i, !j) :: acc else acc)
          in
          go 0 []
        in
        (* (first column, last one + 1, top row, rows) *)
        let blocks =
          List.fold_left
            (fun acc y ->
              List.fold_left
                (fun acc (i, j) ->
                  match List.partition (fun (i', j', top, n) -> i' = i && j' = j && top +.. n = y) acc with
                  | [ (_, _, top, n) ], others -> (i, j, top, n +.. 1) :: others
                  | _ -> (i, j, y, 1) :: acc)
                acc (spans y))
            [] (List.init h Fun.id)
        in
        List.map
          (fun (i, j, top, n) ->
            let bw = float_of_int (j -.. i) and bh = float_of_int n in
            rectangle color (bw * px) (bh * px)
            |> move ((float_of_int i + (bw / 2.) - (float_of_int w / 2.)) * px) (((float_of_int h / 2.) - float_of_int top - (bh / 2.)) * px))
          blocks
        @ layers later
  in
  group (layers palette)

(* Link: down, down walking, up, up walking, right, right walking; left
 * is right flipped *)
let link_palette, link_sheet = Sprite.of_xpm Lttp_xpm.link
let link_rows = frames 16 link_sheet
let link_right = Array.map (paint link_palette) link_rows
let link_left = Array.map (fun rows -> paint link_palette (Sprite.flip rows)) link_rows

let soldier_palette, soldier_sheet = Sprite.of_xpm Lttp_xpm.soldier
let soldier_rows = frames 16 soldier_sheet
let soldier_down = paint soldier_palette soldier_rows.(0)
let soldier_up = paint soldier_palette soldier_rows.(1)
let soldier_right = paint soldier_palette soldier_rows.(2)
let soldier_left = paint soldier_palette (Sprite.flip soldier_rows.(2))

let tree = let palette, rows = Sprite.of_xpm Lttp_xpm.tree in paint palette rows
let pedestal = let palette, rows = Sprite.of_xpm Lttp_xpm.pedestal in paint palette rows

let tiles_palette, tiles_sheet = Sprite.of_xpm Lttp_xpm.tiles

(* the ground's green, drawn once under the whole map; a tile draws
 * without it *)
let grass_green = List.assoc 'g' tiles_palette

(* [on_base rows]: a tile painted from its most common colour, first:
 * one square, the rest over it -- water is a square and its waves. A
 * tile with grass in it (a bush, the flowers) leaves the grass's green
 * out, never painted: it is the rectangle under the map. *)
let on_base (rows : string list) : shape =
  let chars = List.concat_map (fun r -> List.init (String.length r) (String.get r)) rows in
  let count c = List.length (List.filter (( = ) c) chars) in
  let base = if List.mem 'g' chars then 'g' else List.fold_left (fun best c -> if count c > count best then c else best) (List.hd chars) chars in
  let rest = List.filter (fun (ch, _) -> ch <> base) tiles_palette in
  paint (if base = 'g' then rest else (base, List.assoc base tiles_palette) :: rest) rows

let tiles = Array.map on_base (frames 16 tiles_sheet)

let items_palette, items_sheet = Sprite.of_xpm Lttp_xpm.items
let items = frames 16 items_sheet
let rupee = paint items_palette items.(0)
let heart = paint items_palette items.(1)

(* the three pendants, one drawing, recoloured: courage, power, wisdom *)
let pendant (p : char) : shape =
  let light, dark = match p with '1' -> (rgb 64 192 80, rgb 24 112 40) | '2' -> (rgb 64 112 232, rgb 24 48 144) | _ -> (rgb 232 64 64, rgb 144 24 24) in
  paint (List.map (fun (ch, c) -> match ch with 'P' -> (ch, light) | 'p' -> (ch, dark) | _ -> (ch, c)) items_palette) items.(2)

let pendants = [ ('1', pendant '1'); ('2', pendant '2'); ('3', pendant '3') ]

(* the ground, flat: the tiles, and what lies on them *)
let ground_tile (c : char) : shape =
  match c with
  | 'v' -> tiles.(0)
  | ',' -> tiles.(1)
  | '.' -> tiles.(2)
  | '~' -> tiles.(3)
  | '^' -> tiles.(4)
  | '#' -> tiles.(5)
  | '=' -> tiles.(6)
  | '*' -> tiles.(7)
  | 'o' -> tiles.(8)
  | 'H' -> tiles.(9)
  | 'w' -> tiles.(10)
  | '|' -> tiles.(11)
  | 'r' -> rupee
  | 'h' -> heart
  | '1' | '2' | '3' -> List.assoc c pendants
  | _ -> group []

(* the tile shapes are made once, not every frame for every cell *)
let ground_shapes = Array.init 256 (fun i -> ground_tile (Char.chr i))
let ground (c : char) : shape = ground_shapes.(Char.code c)

(* The shadows, the map drawn a second time with a shadow for a picture:
 * the light comes from the top left, as in all of the original's art,
 * so a shadow falls down and to the right of what throws it -- under a
 * tree's canopy, a bush, a rock, the pedestal; on the ground at the foot
 * of the cliff's face; on the water under the bridge. Drawn over the
 * ground tiles, so a bush's own lower right is darkened a little too,
 * which is how the original shades it. *)
let cast_shadow (c : char) : shape =
  let dark w h = oval black w h |> fade 0.3 in
  let band h = rectangle black tile h |> fade 0.28 |> move_y ((-.tile / 2.) - (h / 2.)) in
  match c with
  | 'T' -> dark 96. 30. |> move 12. ((-.tile / 2.) + 4.)
  | 'M' -> dark 64. 20. |> move 8. ((-.tile / 2.) + 4.)
  | '*' | 'o' -> dark 42. 14. |> move 6. ((-.tile / 2.) + 3.)
  | '#' -> band 18.
  | 'H' -> band 15.
  | _ -> group []

let shadow_shapes = Array.init 256 (fun i -> cast_shadow (Char.chr i))
let shadows (c : char) : shape = shadow_shapes.(Char.code c)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the trick of this game, in 7 lines (see the header): a thing
 * standing on the ground is its picture, [rows] pixels high, its
 * bottom on its base line; and the standing things are drawn farthest
 * first, the farther the higher up the screen their base *)
type standing = { base : number; picture : shape }

let standing (rows : int) (picture : shape) (x : number) (base : number) : standing =
  { base; picture = picture |> move x (base + (float_of_int rows * px / 2.)) }

let draw_standing (things : standing list) : shape list =
  List.map (fun t -> t.picture) (List.sort (fun a b -> compare b.base a.base) things)

(* the trees and the pedestal, standing on the bottom of their cell;
 * they don't move: found once, and drawn when near the screen *)
let trees = List.map (fun p -> ('T', p)) (Tilemap.find level 'T') @ List.map (fun p -> ('M', p)) (Tilemap.find level 'M')

let fixed (r : Camera2d.rect) : standing list =
  List.filter_map
    (fun (c, (col, row)) ->
      let x, y = Tilemap.center level col row in
      if x < r.left - tile || x > r.right + tile || y < r.bottom - tile || y > r.top + (3. * tile) then None
      else if c = 'T' then Some (standing 40 tree x (y - (tile / 2.)))
      else Some (standing 32 pedestal x (y - (tile / 2.))))
    trees

(* the sword, a blade turning across the swing, from the side Link
 * holds it to the other *)
let blade = group [ rectangle (rgb 240 248 255) 7. 36. |> move_y 22.; rectangle (rgb 64 96 200) 18. 6. |> move_y 4. ]

let sword (g : game) : shape =
  let fx, fy = g.facing in
  let toward = Float.atan2 fy fx * 180. / Float.pi in
  let t = float_of_int (16 -.. g.swing) / 16. in
  blade |> rotate (toward - 90. + 80. - (160. * t)) |> move_y 33.

let link_shape (g : game) : shape =
  let walking = if g.steps /.. 8 mod 2 = 0 then 0 else 1 in
  let body =
    match g.facing with
    | 0., d when d < 0. -> link_right.(walking)
    | 0., _ -> link_right.(2 +.. walking)
    | d, _ when d > 0. -> link_right.(4 +.. walking)
    | _ -> link_left.(4 +.. walking)
  in
  let body = body |> move_y (22. * px / 2.) in
  (* the blade behind him when he strikes north, in front otherwise *)
  if g.swing = 0 then body
  else
    let s = sword g in
    if snd g.facing > 0. then group [ s; body ] else group [ body; s ]

let soldier_shape (frames : int) (s : soldier) : shape =
  let fx, fy = s.dir in
  let body =
    if Float.abs fx > Float.abs fy then if fx > 0. then soldier_right else soldier_left else if fy > 0. then soldier_up else soldier_down
  in
  (* a step: one pixel up and down *)
  let bob = if frames /.. 10 mod 2 = 0 then 0. else px in
  body |> move_y ((22. * px / 2.) + bob) |> fade (if s.knock > 0 && s.knock mod 4 < 2 then 0.4 else 1.)

(* a shadow under what walks: grounds it, on a map where nothing has a
 * height *)
let shadow (x : number) (y : number) : shape = oval black 36. 14. |> fade 0.25 |> move x (y - 8.)

let view_leaves (frames : int) ((x, y, t) : number * number * int) : shape list =
  let age = float_of_int (frames -.. t) in
  List.init 6 (fun i ->
      let a = float_of_int i * 60. * Float.pi / 180. in
      rectangle (rgb 72 176 48) 9. 9. |> rotate (age * 20.) |> move (x + (Float.cos a * age * 3.)) (y + (Float.sin a * age * 3.) + (age * 1.5)) |> fade (1. - (age / 24.)))

let view_world (g : game) : shape =
  let r = Camera2d.visible screen0 g.cam in
  let ground_layer =
    [ rectangle grass_green (bounds.right - bounds.left) (bounds.top - bounds.bottom); Tilemap.view_visible r ground g.map;
      Tilemap.view_visible r shadows g.map ]
    @ List.map (fun s -> shadow s.sx s.sy) g.soldiers
    @ [ shadow g.x g.y ]
  in
  let link = if g.hurt > 0 && g.hurt mod 8 < 4 then [] else [ standing 0 (link_shape g) g.x (g.y - 12.) ] in
  let soldiers = List.map (fun s -> standing 0 (soldier_shape g.frames s) s.sx (s.sy - 12.)) g.soldiers in
  Camera2d.view g.cam (ground_layer @ draw_standing (fixed r @ soldiers @ link) @ List.concat_map (view_leaves g.frames) g.leaves)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the top of the screen: life in hearts (half ones too), rupees, the
 * pendants found *)
let view_hud (g : game) : shape list =
  let hearts =
    List.init (g.max_life /.. 2) (fun i ->
        let left = g.life -.. (i *.. 2) in
        let x = 250. + (float_of_int i * 50.) in
        (if left >= 2 then heart else if left = 1 then group [ heart; rectangle (rgb 60 20 20) 18. 36. |> move 9. 0. ] else heart |> fade 0.25)
        |> move x 440.)
  in
  [ rectangle black 1000. 100. |> move_y 450. |> fade 0.6; text white 2.5 "- LIFE -" |> move 300. 475.; rupee |> move (-430.) 440.;
    text white 3. (Printf.sprintf "%03d" g.rupees) |> move (-370.) 440. ]
  @ hearts
  @ List.mapi (fun i p -> List.assoc p pendants |> move (-200. + (float_of_int i * 50.)) 440.) (List.rev g.pendants)
  @
  if at_pedestal g && List.length g.pendants < 3 then [ text white 2.5 "THE SWORD WILL NOT MOVE. THREE PENDANTS OF VIRTUE..." |> move_y 360. ] else []

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text (rgb 240 200 40) 6. "A LINK TO THE PAST" |> move_y 200.; text white 2.5 "arrows walk   space swings the sword" |> move_y 110.;
        text white 2.5 "find the three pendants, and pull the Master Sword" |> move_y 70.; pedestal |> scale 1.5 |> move (-120.) (-100.);
        link_right.(0) |> scale 1.5 |> move 60. (-100.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-300.) ]
  | Playing g -> view_world g :: view_hud g
  | Won frames ->
      [ text (rgb 255 220 60) 5. "THE MASTER SWORD!" |> move_y 60.; text white 3. (Printf.sprintf "%.0f seconds" (float_of_int frames / 60.)) |> move_y (-40.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over -> [ text red 6. "GAME OVER" ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
