(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyScratch: programming by snapping blocks together (Scratch,
 * Mitchel Resnick, John Maloney, Natalie Rusk, Evelyn Eastmond, Amon
 * Millner and the Lifelong Kindergarten group, MIT Media Lab, 2007;
 * this is the look of Scratch 2.0, 2013, the one in the browser).
 *
 * Scratch is Logo's grandchild (Seymour Papert, 1967: the turtle is a
 * sprite with a pen) by way of Etoys (Alan Kay, Squeak, 1997, where
 * scripts were tiles dragged onto objects) -- and the first Scratch
 * was itself written in Squeak, Smalltalk's heir (TinySmalltalk80).
 * What it added is what made tens of millions of children program:
 *
 * - **no syntax errors**: a program is blocks whose shapes say where
 *   they fit -- a notch above, a tab below, a mouth, a round or a
 *   pointed slot -- so every program that can be built can run
 *   (libs/languages/scratch, Scratch_blocks.mli). The blocks are one
 *   table, which the palette, the editor, the runtime and the text
 *   all read;
 * - **tinkering**: click any script, even while the project runs, and
 *   it runs; change a number, click again. Nothing is compiled, nothing
 *   is saved first;
 * - **concurrency for free**: every script is a thread, started by its
 *   hat (the green flag, a key, a click, a message), and a thread
 *   yields only at the end of a loop's turn or in a wait, then the
 *   stage is drawn -- so a forever loop is an animation and two sprites
 *   dancing together need no locks (Scratch_run.mli);
 * - **the stage**: sprites on a 480 x 360 plane, x right and y up,
 *   directions a compass's, the pen of Logo's turtle.
 *
 * The text under the sprites is the current sprite's scripts in the
 * scratchblocks notation (Scratch_text.mli), the forums' way of
 * writing blocks, printed from the blocks as they change: the same
 * program as text, which is what TinyBasic and TinyTurboPascal have
 * instead of blocks.
 *
 * The project it opens is the first one children make: the cat that
 * walks and bounces, and says hello; and a pencil sprite drawing a
 * flower of squares, a turtle-graphics classic, at the same time.
 * Click the green flag.
 *
 * Mouse: drag a block from the palette into the scripts area (a stack
 * snaps under a block, into a mouth, or above a script; a reporter
 * into a slot); drag a block in a script and it takes those under it
 * along; drop anything on the palette to delete it; click a script to
 * run it; click a slot and type (Enter). The wheel scrolls the palette
 * or the scripts. Click a sprite on the stage for "when this sprite
 * clicked", a thumbnail below the stage to edit its scripts. Keys go
 * to the project ("when [space] key pressed", "key [space] pressed?").
 * Flags: sprite=pencil opens the pencil's scripts; run=on clicks the
 * green flag at the start.
 *
 * What it uses: libs/languages/scratch (Scratch_blocks, Scratch_text,
 * Scratch_run) and appkits/blocks (Block_layout, Block_edit). Not gui/:
 * the blocks are drawn from Block_layout's pieces, the panes by hand.
 *
 * The trick of this app, a departure from Scratch: the stage steps
 * every other frame of the Playground (Scratch 2's 30 a second), its
 * clock counting those steps rather than reading the time, so that a
 * run is the same run every time -- wait and glide included.
 *
 * What it deliberately does not do: sounds (the Sound category and the
 * cat's meow); the paint editor, costumes drawn by the user (the
 * costumes here are shapes); the stage's own scripts and backdrops;
 * lists; clones (Scratch 2's); "ask and wait"; the "more blocks" of
 * Scratch 2, procedures of one's own (BYOB, then Snap!, went further:
 * blocks as first-class values, lambda); saving and sharing projects
 * -- the online community that was half of Scratch.
 *
 * Exercises: save the project in the store as scratchblocks text, one
 * section a sprite, and load it back (Scratch_text reads it already);
 * a context menu on a block: duplicate, delete; right-click a
 * reporter in the palette to see its value, as Scratch shows in a
 * bubble; "ask [] and wait" with a text field on the stage; clones.
 *)
open Playground
module B = Scratch_blocks
module R = Scratch_run
module L = Block_layout
module E = Block_edit

(* a text's width at the blocks' size, 12, as the software renderer
   draws it (Hershey's Futura, graphics/font); the other backends'
   fonts are near enough *)
let font_size = 12.
let measure s = snd (Hershey.layout s) *. font_size /. Hershey.units_per_em

(*****************************************************************************)
(* The project *)
(*****************************************************************************)

let cat_scripts =
  {|when flag clicked
go to x: (-120) y: (-80)
point in direction (90)
say [Hello!] for (1) seconds
forever
  move (4) steps
  next costume
  if on edge, bounce
end

when this sprite clicked
change size by (10)
say (join [I am ] (size)) for (1) seconds

when [space v] key pressed
turn right (15) degrees|}

let pencil_scripts =
  {|when flag clicked
clear
go to x: (0) y: (20)
point in direction (90)
set pen color to (0)
pen down
repeat (36)
  repeat (4)
    move (70) steps
    turn right (90) degrees
  end
  turn right (10) degrees
  change pen color by (5)
end
pen up|}

(* the scripts area's top-left, where a project's scripts are put in
   a column *)
let scripts_left = 110.
let scripts_top = 420.

let column scripts =
  List.rev
    (fst
       (List.fold_left
          (fun (acc, y) (s : B.script) -> ({ s with x = scripts_left; y } :: acc, y -. L.height ~measure s.blocks -. 30.))
          ([], scripts_top) scripts))

let parse text = match Scratch_text.parse text with Ok s -> column s | Error e -> failwith e

let project () =
  let cat = R.sprite ~name:"Cat" ~costumes:2 ~radius:34. (parse cat_scripts) in
  let pencil = R.sprite ~name:"Pencil" ~costumes:1 ~radius:14. (parse pencil_scripts) in
  R.stage [ { cat with x = -120.; y = -80. }; { pencil with x = 0.; y = 20.; pen_hue = 0. } ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type drag = { dragged : E.dragged; grab : float * float (* the mouse from the dragged's top-left *); at : float * float }

type model = {
  stage : R.t;
  current : string; (* the sprite whose scripts are shown *)
  category : B.category;
  palette_scroll : float;
  scroll : float;
  press : (L.path * (float * float)) option; (* a block pressed, not yet dragged *)
  drag : drag option;
  editing : (L.path * string * bool) option; (* a slot being typed in; still all selected, the first key replacing it *)
  frame : int;
  started : bool;
  was : string list;
  was_down : bool;
}

let initial =
  {
    stage = project ();
    current = "Cat";
    category = B.Motion;
    palette_scroll = 0.;
    scroll = 0.;
    press = None;
    drag = None;
    editing = None;
    frame = 0;
    started = false;
    was = [];
    was_down = false;
  }

(* the screen: Scratch 2's -- the stage top left, the sprites under it,
   the palette in the middle, the scripts on the right *)
let stage_cx = -315.
let stage_cy = 300.
let stage_k = 0.75
let stage_top = 435.
let stage_bottom = 165.
let stage_left = -495.
let stage_right = -135.
let palette_left = -130.
let palette_right = 95.
let blocks_top = 345.
let top_bar = 470.
let in_stage (x, y) = x > stage_left && x < stage_right && y < stage_top && y > stage_bottom
let in_palette (x, y) = x > palette_left && x < palette_right && y < top_bar
let in_scripts (x, y) = x >= palette_right && y < top_bar
let to_stage (x, y) = ((x -. stage_cx) /. stage_k, (y -. stage_cy) /. stage_k)
let of_stage (x, y) = (stage_cx +. (x *. stage_k), stage_cy +. (y *. stage_k))
let flag_button = (-200., 452.)
let stop_button = (-160., 452.)
let near (ax, ay) (bx, by) r = Float.hypot (ax -. bx) (ay -. by) < r

let sprite_of m = R.find m.stage m.current
let scripts_of m = (sprite_of m).scripts
let set_scripts m scripts = { m with stage = R.update m.stage { (sprite_of m) with scripts } }

(* the scripts where they are drawn, the scroll applied *)
let shown m = List.map (fun (s : B.script) -> { s with y = s.y +. m.scroll }) (scripts_of m)

(* the palette's blocks, each where it is drawn *)
let palette m =
  let variables = "score" :: List.filter (( <> ) "score") (List.map fst m.stage.vars) in
  let blocks =
    List.concat_map
      (fun (s : B.spec) ->
        if s.category <> m.category then []
        else if s.op = "data_variable" then List.map B.variable (List.sort_uniq compare variables)
        else [ B.make s.op ])
      B.specs
  in
  List.rev
    (fst
       (List.fold_left
          (fun (acc, y) b ->
            let _, h = L.size ~measure b in
            ((b, (palette_left +. 10., y)) :: acc, y -. h -. 10.))
          ([], blocks_top +. m.palette_scroll) blocks))

let thumbnails m = List.mapi (fun i (s : R.sprite) -> (s.name, (stage_left +. 50. +. (float_of_int i *. 95.), 110.))) m.stage.sprites

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the Playground's key names as Scratch's *)
let scratch_key k =
  match k with
  | " " -> Some "space"
  | "ArrowLeft" -> Some "left arrow"
  | "ArrowRight" -> Some "right arrow"
  | "ArrowUp" -> Some "up arrow"
  | "ArrowDown" -> Some "down arrow"
  | k when String.length k = 1 -> Some (String.lowercase_ascii k)
  | _ -> None

(* the slot typed in, set *)
let commit m =
  match m.editing with Some (path, text, _) -> { (set_scripts m (E.set_text (scripts_of m) path text)) with editing = None } | None -> m

(* where a dragged thing lands *)
let release m d =
  let x, y = d.at in
  let gx, gy = d.grab in
  let corner = (x -. gx, y -. gy) in
  if in_palette (x, y) then m (* dropped on the palette: deleted *)
  else
    let scripts = shown m in
    let unscroll (s : B.script) = { s with y = s.y -. m.scroll } in
    let cx, cy = corner in
    let cx = Float.max (palette_right +. 5.) cx in
    let result =
      match d.dragged with
      | E.Stack blocks -> (
          match L.snap ~measure scripts blocks ~at:corner with
          | Some target -> E.drop ~measure scripts blocks target
          | None -> E.alone scripts (cx, cy) blocks)
      | E.Reporter r -> (
          let pieces, _ = L.layout ~measure scripts in
          let _, h = L.size ~measure r in
          match L.slot_at pieces (fst corner +. 4., snd corner -. (h /. 2.)) with
          | Some path -> E.drop_in_slot scripts path r
          | None -> E.alone scripts (cx, cy) [ r ])
    in
    set_scripts m (List.map unscroll result)

let press_at m p =
  let m = commit m in
  if near p flag_button 14. then { m with stage = R.green_flag m.stage }
  else if near p stop_button 14. then { m with stage = R.stop m.stage }
  else if in_stage p then
    let sx, sy = to_stage p in
    let hit = List.find_opt (fun (s : R.sprite) -> s.visible && Float.hypot (s.x -. sx) (s.y -. sy) < s.radius *. s.size /. 100.) (List.rev m.stage.sprites) in
    match hit with Some s -> { m with stage = R.click s.name m.stage } | None -> m
  else
    match List.find_opt (fun (_, xy) -> near p xy 40.) (thumbnails m) with
    | Some (name, _) -> { m with current = name; scroll = 0.; editing = None }
    | None -> (
        let x, y = p in
        let category =
          List.find_opt
            (fun (i, _) ->
              let cx = if i mod 2 = 0 then -70. else 38. and cy = 425. -. (float_of_int (i / 2) *. 22.) in
              Float.abs (x -. cx) < 52. && Float.abs (y -. cy) < 10.)
            (List.mapi (fun i c -> (i, c)) B.categories)
        in
        match category with
        | Some (_, c) -> { m with category = c; palette_scroll = 0. }
        | None ->
            if in_palette p then
              match List.find_opt (fun (b, (bx, by)) -> let w, h = L.size ~measure b in x >= bx && x <= bx +. w && y <= by && y >= by -. h) (palette m) with
              | Some (b, (bx, by)) ->
                  let dragged = if (B.spec b.op).shape = B.Reporter || (B.spec b.op).shape = B.Predicate then E.Reporter b else E.Stack [ b ] in
                  { m with drag = Some { dragged; grab = (x -. bx, y -. by); at = p } }
              | None -> m
            else if in_scripts p then
              let pieces, _ = L.layout ~measure (shown m) in
              match L.slot_at pieces p with
              | Some path -> { m with editing = Some (path, Option.value ~default:"" (E.text (scripts_of m) path), true) }
              | None -> ( match L.block_at pieces p with Some path -> { m with press = Some (path, p) } | None -> m)
            else m)

(* a pressed block moved far enough: taken, with what is under it *)
let start_drag m p =
  match m.press with
  | Some (path, (px, py)) when not (near p (px, py) 4.) -> (
      let pieces, _ = L.layout ~measure (shown m) in
      let corner = List.find_map (function L.Body b when b.path = path -> Some (b.x, b.y) | _ -> None) pieces in
      match (E.take (scripts_of m) path, corner) with
      | Some (dragged, rest), Some (bx, by) -> { (set_scripts m rest) with press = None; drag = Some { dragged; grab = (px -. bx, py -. by); at = p } }
      | _ -> { m with press = None })
  | _ -> m

let step_stage m (computer : computer) keys =
  let mx, my = to_stage (computer.mouse.mx, computer.mouse.my) in
  let input = { R.mouse_x = mx; mouse_y = my; mouse_down = computer.mouse.mdown && in_stage (computer.mouse.mx, computer.mouse.my); keys; time = float_of_int (m.frame / 2) /. 30. } in
  (* Scratch 2's 30 frames a second, the Playground's 60 *)
  if m.frame mod 2 = 0 then { m with stage = R.step input m.stage } else m

let update (computer : computer) m =
  let m =
    if m.started then m
    else
      let m = { m with started = true } in
      let m = if List.assoc_opt "sprite" computer.flags = Some "pencil" then { m with current = "Pencil" } else m in
      if List.assoc_opt "run" computer.flags = Some "on" then { m with stage = R.green_flag m.stage } else m
  in
  let mouse = computer.mouse in
  let p = (mouse.mx, mouse.my) in
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.was) in
  (* the keys: to the slot being typed in, else to the project *)
  let m =
    match m.editing with
    | Some (path, text, all) -> (
        let typed = String.concat "" (List.map (String.make 1) (List.filter (fun c -> c >= ' ') (List.of_seq (String.to_seq computer.keyboard.typed)))) in
        if pressed "Enter" then commit m
        else if pressed "Escape" then { m with editing = None }
        else if pressed "Backspace" then { m with editing = Some (path, (if all || text = "" then "" else String.sub text 0 (String.length text - 1)), false) }
        else match typed with "" -> m | typed -> { m with editing = Some (path, (if all then typed else text ^ typed), false) })
    | None -> List.fold_left (fun m k -> match scratch_key k with Some k when pressed (List.find (fun k' -> scratch_key k' = Some k) now) -> { m with stage = R.key k m.stage } | _ -> m) m now
  in
  let keys = if m.editing = None then List.filter_map scratch_key now else [] in
  let m =
    if mouse.mwheel = 0. then m
    else if in_palette p then { m with palette_scroll = Float.max 0. (m.palette_scroll -. (mouse.mwheel *. 30.)) }
    else if in_scripts p then { m with scroll = Float.max 0. (m.scroll -. (mouse.mwheel *. 30.)) }
    else m
  in
  let m =
    if mouse.mdown && not m.was_down then press_at m p
    else if mouse.mdown then match m.drag with Some d -> { m with drag = Some { d with at = p } } | None -> start_drag m p
    else if m.was_down then
      match (m.drag, m.press) with
      | Some d, _ -> release { m with drag = None } { d with at = p }
      | None, Some (path, _) ->
          (* a click, not a drag: the script runs *)
          { m with press = None; stage = R.run_script m.current path.script m.stage }
      | None, None -> m
    else m
  in
  let m = step_stage m computer keys in
  { m with frame = m.frame + 1; was = now; was_down = mouse.mdown }

(*****************************************************************************)
(* Drawing blocks *)
(*****************************************************************************)

let color_of = function
  | B.Motion -> (74., 108., 212.)
  | B.Looks -> (138., 85., 215.)
  | B.Events -> (200., 131., 48.)
  | B.Control -> (225., 169., 26.)
  | B.Sensing -> (44., 165., 226.)
  | B.Operators -> (92., 183., 18.)
  | B.Variables -> (238., 125., 22.)
  | B.Pen -> (14., 154., 108.)

let shade k (r, g, b) = rgb (int_of_float (r *. k)) (int_of_float (g *. k)) (int_of_float (b *. k))
let text color size s (x, y) = words color s |> scale (size /. words_font_size) |> move x y

(* a word from its left edge, at its estimated width *)
let label color s (x, y) = text color font_size s (x +. (measure s /. 2.), y)

let round_box color w h (x, y) =
  let r = h /. 2. in
  [ rectangle color (Float.max 0. (w -. h)) h |> move (x +. (w /. 2.)) (y -. r); circle color r |> move (x +. r) (y -. r); circle color r |> move (x +. w -. r) (y -. r) ]

let hexagon color w h (x, y) = polygon color [ (x, y -. (h /. 2.)); (x +. (h /. 2.), y); (x +. w -. (h /. 2.), y); (x +. w, y -. (h /. 2.)); (x +. w -. (h /. 2.), y -. h); (x +. (h /. 2.), y -. h) ]

(* the jigsaw's outline: a notch in the top, a tab under the bottom *)
let notch x y = [ (x +. 10., y); (x +. 14., y -. 4.); (x +. 26., y -. 4.); (x +. 30., y) ]
let tab x y = [ (x +. 30., y); (x +. 26., y -. 4.); (x +. 14., y -. 4.); (x +. 10., y) ]

let outline (s : B.spec) x y w h mouths =
  let bottom = y -. h in
  let top = if s.shape = B.Hat then [ (x, y -. L.arm); (x +. w, y -. L.arm) ] else [ (x, y) ] @ notch x y @ [ (x +. w, y) ] in
  let arms =
    List.concat
      (List.mapi
         (fun i (mt, mh) ->
           let mb = mt -. mh in
           let next_top = match List.nth_opt mouths (i + 1) with Some (t, _) -> t | None -> bottom +. L.arm in
           [ (x +. w, mt) ] @ tab (x +. L.arm) mt @ [ (x +. L.arm, mt); (x +. L.arm, mb) ] @ List.rev (notch (x +. L.arm) mb) @ [ (x +. w, mb); (x +. w, next_top) ])
         mouths)
  in
  let under = if s.shape = B.Cap || s.shape = B.C_cap then [] else tab x bottom in
  top @ arms @ [ (x +. w, bottom) ] @ under @ [ (x, bottom) ]

let body (s : B.spec) x y w h mouths =
  let c = color_of s.category in
  let draw k dy =
    match s.shape with
    | B.Reporter -> round_box (shade k c) w h (x, y +. dy)
    | B.Predicate -> [ hexagon (shade k c) w h (x, y +. dy) ]
    | B.Hat ->
        [ oval (shade k c) 80. 34. |> move (x +. 40.) (y -. L.arm +. dy); polygon (shade k c) (List.map (fun (a, b) -> (a, b +. dy)) (outline s x y w h mouths)) ]
    | _ -> [ polygon (shade k c) (List.map (fun (a, b) -> (a, b +. dy)) (outline s x y w h mouths)) ]
  in
  draw 0.72 (-1.5) @ draw 1. 0.

(* a block's pieces as shapes, the slot being typed in with its caret *)
let pieces_shapes ?(editing = None) pieces =
  List.concat_map
    (function
      | L.Body { spec; x; y; w; h; mouths; _ } -> body spec x y w h mouths
      | L.Label { x; y; text = t } -> [ label white t (x, y) ]
      | L.Slot { path; part; x; y; w; h; text = t } -> (
          let t = match editing with Some (p, typed, _) when p = path -> typed ^ "|" | _ -> t in
          let cx = x +. (w /. 2.) and cy = y -. (h /. 2.) in
          match part with
          | B.Num _ -> round_box white w h (x, y) @ [ text black 12. t (cx, cy) ]
          | B.Menu _ -> [ rectangle black w h |> move cx cy |> fade 0.25; text white 12. t (cx -. 4., cy); triangle white 3. |> rotate 180. |> move (x +. w -. 6.) (cy -. 1.) ]
          | B.Bool -> [ hexagon (rgb 0 0 0) w h (x, y) |> fade 0.2 ]
          | _ -> [ rectangle white w h |> move cx cy; text black 12. t (cx, cy) ]))
    pieces

(*****************************************************************************)
(* Drawing the stage *)
(*****************************************************************************)

(* Scratch 2's pen colour: 0 to 200 once round the hues *)
let hue_color h =
  let h = Float.rem (h *. 1.8) 360. /. 60. in
  let x = 1. -. Float.abs (Float.rem h 2. -. 1.) in
  let r, g, b = match int_of_float h with 0 -> (1., x, 0.) | 1 -> (x, 1., 0.) | 2 -> (0., 1., x) | 3 -> (0., x, 1.) | 4 -> (x, 0., 1.) | _ -> (1., 0., x) in
  rgb (int_of_float (r *. 255.)) (int_of_float (g *. 255.)) (int_of_float (b *. 255.))

let segment color width (ax, ay) (bx, by) =
  let len = Float.hypot (bx -. ax) (by -. ay) in
  if len < 0.01 then circle color (width /. 2.) |> move ax ay
  else
    rectangle color (len +. width) width
    |> rotate (Float.atan2 (by -. ay) (bx -. ax) *. 180. /. Float.pi)
    |> move ((ax +. bx) /. 2.) ((ay +. by) /. 2.)

(* the costumes, facing right (left when [flip]: the rotation style
   left-right), in stage units round the sprite's middle; the cat's
   two, its legs apart and together *)
let costume ?(flip = false) (s : R.sprite) =
  let orange = rgb 250 165 40 and dark = rgb 60 40 20 and cream = rgb 255 240 220 in
  let fx x = if flip then -.x else x in
  let at x y sh = move (fx x) y sh in
  let poly c pts = polygon c (List.map (fun (x, y) -> (fx x, y)) pts) in
  match s.name with
  | "Pencil" ->
      (* the tip at the middle, where the pen draws *)
      [
        poly (rgb 240 200 120) [ (0., 0.); (8., 5.); (8., -5.) ];
        poly (rgb 250 200 40) [ (8., 5.); (40., 5.); (40., -5.); (8., -5.) ];
        rectangle (rgb 240 130 160) 6. 10. |> at 43. 0.;
        circle dark 1.5;
      ]
  | _ ->
      let legs = if s.costume = 0 then [ (-14., -4.); (-6., 4.); (6., -4.); (14., 4.) ] else [ (-12., 0.); (-8., 0.); (8., 0.); (12., 0.) ] in
      List.map (fun (lx, tilt) -> rectangle orange 6. 18. |> rotate (fx (tilt *. 3.)) |> at lx (-20.)) legs
      @ [
          poly orange [ (-22., 3.); (-37., 18.); (-31., 22.); (-19., -3.) ];
          poly orange [ (-37., 18.); (-33., 29.); (-27., 27.); (-31., 17.) ];
          oval orange 48. 28. |> at (-4.) (-6.);
          poly orange [ (8., 20.); (12., 36.); (20., 24.) ];
          poly orange [ (22., 24.); (32., 36.); (32., 18.) ];
          circle orange 17. |> at 20. 10.;
          oval cream 20. 12. |> at 26. 2.;
          oval white 8. 10. |> at 16. 14.;
          oval white 8. 10. |> at 26. 14.;
          circle black 2. |> at 17. 14.;
          circle black 2. |> at 27. 14.;
          circle dark 2. |> at 30. 5.;
        ]

let sprite_shapes (s : R.sprite) =
  let x, y = of_stage (s.x, s.y) in
  let k = stage_k *. s.size /. 100. in
  let flip = s.rotation = R.Left_right && s.direction < 0. in
  let turn = match s.rotation with R.All_around -> 90. -. s.direction | _ -> 0. in
  [ group (costume ~flip s) |> rotate turn |> scale k |> move x y ]

let bubble (s : R.sprite) =
  match s.bubble with
  | Some said when s.visible ->
      let x, y = of_stage (s.x, s.y) in
      let r = s.radius *. s.size /. 100. *. stage_k in
      let w = Float.max 40. ((float_of_int (String.length said) *. 7.) +. 20.) in
      let bx = x +. (r *. 0.6) and by = y +. r +. 22. in
      [
        polygon (rgb 160 160 160) [ (bx +. 8., by -. 14.); (bx +. 20., by -. 14.); (bx +. 4., by -. 26.) ];
        rectangle (rgb 160 160 160) (w +. 2.) 30. |> move (bx +. (w /. 2.)) by;
        rectangle white w 28. |> move (bx +. (w /. 2.)) by;
        polygon white [ (bx +. 9., by -. 13.); (bx +. 19., by -. 13.); (bx +. 5., by -. 23.) ];
        text black 13. said (bx +. (w /. 2.), by);
      ]
  | _ -> []

let stage_shapes m =
  let ink =
    List.rev_map
      (function
        | R.Line (a, b, hue, size) -> segment (hue_color hue) (Float.max 1. (size *. stage_k)) (of_stage a) (of_stage b)
        | R.Stamp s -> group (sprite_shapes s))
      m.stage.ink
  in
  [ rectangle white (480. *. stage_k) (360. *. stage_k) |> move stage_cx stage_cy ]
  @ ink
  @ List.concat_map (fun (s : R.sprite) -> if s.visible then sprite_shapes s else []) m.stage.sprites
  @ List.concat_map bubble m.stage.sprites

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view (_ : computer) m =
  let grey = rgb 230 232 235 and line = rgb 200 200 205 and dark = rgb 90 90 95 in
  let scripts = shown m in
  let pieces, _ = L.layout ~measure scripts in
  (* the running scripts glow *)
  let glow =
    List.concat_map
      (fun (th : R.thread) ->
        if th.sprite <> m.current then []
        else
          match List.nth_opt scripts th.script with
          | Some sc ->
              let w = List.fold_left (fun acc b -> Float.max acc (fst (L.size ~measure b))) 0. sc.blocks and h = L.height ~measure sc.blocks in
              [ rectangle (rgb 255 236 110) (w +. 12.) (h +. 12.) |> move (sc.x +. (w /. 2.)) (sc.y -. (h /. 2.)) |> fade 0.7 ]
          | None -> [])
      m.stage.threads
  in
  let scripts_pane = [ rectangle (rgb 242 242 242) (500. -. palette_right) 1000. |> move ((palette_right +. 500.) /. 2.) 0. ] @ glow @ pieces_shapes ~editing:m.editing pieces in
  let palette_blocks =
    List.concat_map (fun ((b : B.block), (x, y)) -> pieces_shapes (fst (L.layout ~measure [ { x; y; blocks = [ b ] } ]))) (palette m)
  in
  let categories =
    List.concat
      (List.mapi
         (fun i c ->
           let cx = if i mod 2 = 0 then -70. else 38. and cy = 425. -. (float_of_int (i / 2) *. 22.) in
           let on = c = m.category in
           [ rectangle (if on then shade 1. (color_of c) else white) 104. 19. |> move cx cy; rectangle (shade 1. (color_of c)) 6. 19. |> move (cx -. 49.) cy; text (if on then white else dark) 12. (B.category_name c) (cx +. 3., cy) ])
         B.categories)
  in
  let palette_pane = [ rectangle white (palette_right -. palette_left) 1000. |> move ((palette_left +. palette_right) /. 2.) 0. ] @ palette_blocks @ [ rectangle grey (palette_right -. palette_left) (top_bar -. 355.) |> move ((palette_left +. palette_right) /. 2.) ((top_bar +. 355.) /. 2.) ] @ categories in
  let s = sprite_of m in
  let thumbs =
    List.concat_map
      (fun (name, (x, y)) ->
        let sp = R.find m.stage name in
        let k = 30. /. sp.radius in
        [ rectangle (if name = m.current then rgb 120 180 240 else line) 84. 84. |> move x y; rectangle white 80. 80. |> move x y ]
        @ [ group (costume { sp with costume = 0 }) |> scale (k *. 0.9) |> move x (y +. 6.); text dark 11. name (x, y -. 30.) ])
      (thumbnails m)
  in
  let source = String.split_on_char '\n' (Scratch_text.print (scripts_of m)) in
  let source = List.filteri (fun i _ -> i < 26) source in
  let left = [ rectangle grey (stage_right -. stage_left +. 10.) (stage_bottom +. 500.) |> move ((stage_left +. stage_right) /. 2.) ((stage_bottom -. 500.) /. 2.); rectangle grey 10. 1000. |> move (-500.) 0. ]
    @ [ rectangle grey (stage_right -. stage_left +. 10.) (top_bar -. stage_top) |> move ((stage_left +. stage_right) /. 2.) ((top_bar +. stage_top) /. 2.) ]
    @ [ text dark 13. "Sprites" (stage_left +. 30., 158.) ]
    @ thumbs
    @ [ text dark 12. (Printf.sprintf "%s   x: %.0f  y: %.0f  direction: %.0f" s.name s.x s.y s.direction) (stage_cx, 55.) ]
    @ [ rectangle white 350. 470. |> move stage_cx (-205.); text dark 12. "The same scripts as text (scratchblocks)" (stage_cx, 25.) ]
    @ List.mapi
        (fun i l ->
          (* the indentation as an offset, the text from its first word *)
          let t = String.trim l in
          let rec lead i = if i < String.length l && l.[i] = ' ' then lead (i + 1) else i in
          let indent = float_of_int (lead 0) in
          label (rgb 40 40 40) t (stage_left +. 12. +. (indent *. 7.), 5. -. (float_of_int i *. 17.)))
        source
  in
  let fx, fy = flag_button and sx, sy = stop_button in
  let buttons =
    [
      circle (if m.stage.threads <> [] then rgb 200 240 200 else white) 13. |> move fx fy;
      rectangle (rgb 60 60 60) 2. 18. |> move (fx -. 5.) fy;
      polygon (rgb 60 180 60) [ (fx -. 4., fy +. 8.); (fx +. 8., fy +. 4.); (fx -. 4., fy) ];
      circle white 13. |> move sx sy;
      octagon (rgb 220 50 50) 9. |> move sx sy;
    ]
  in
  let top = [ rectangle (rgb 37 160 224) 1000. 30. |> move 0. 485.; text white 16. "TinyScratch" (-440., 485.); text white 13. "File   Edit   Tips" (-320., 485.) ] in
  let dragged =
    match m.drag with
    | Some { dragged; grab = gx, gy; at = x, y } ->
        let blocks = match dragged with E.Stack bs -> bs | E.Reporter r -> [ r ] in
        List.map (fade 0.85) (pieces_shapes (fst (L.layout ~measure [ { x = x -. gx; y = y -. gy; blocks } ])))
    | None -> []
  in
  stage_shapes m @ scripts_pane @ palette_pane @ left @ buttons @ [ text dark 13. m.current (stage_left +. 40., 452.) ] @ top @ dragged

let app = game view update initial
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
