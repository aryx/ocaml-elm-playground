(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scratch_ide.mli *)

open Playground
module B = Scratch_blocks
module R = Scratch_run
module L = Block_layout
module E = Block_edit
module Look = Scratch_look

type colors = {
  pane : color;
  header : color;
  scripts : color;
  side : color;
  sheet : color;
  line : color;
  ink : color;
  sheet_ink : color;
  button_off : color;
}

type config = {
  title : string;
  menus : string;
  bar : color;
  theme : Look.theme;
  colors : colors;
  categories : B.category list;
  palette : R.t -> B.category -> B.block list;
  make_block : bool;
  stage : Look.frame;
  palette_x : float * float;
  scripts_x : float * float;
  flag : float * float;
  stop : float * float;
  name_at : (float * float) option;
}

let measure = Look.measure
let text = Look.text
let label = Look.label

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type drag = { dragged : E.dragged; grab : float * float (* the mouse from the dragged's top-left *); at : float * float }

type model = {
  c : config;
  stage : R.t;
  current : string; (* the sprite whose scripts are shown *)
  category : B.category;
  palette_scroll : float;
  scroll : float;
  press : (L.path * (float * float)) option; (* a block pressed, not yet dragged *)
  drag : drag option;
  editing : (L.path * string * bool) option; (* a slot being typed in; still all selected, the first key replacing it *)
  said : (int * string) option; (* a loose reporter clicked, and its value *)
  frame : int;
  started : bool;
  was : string list;
  was_down : bool;
}

(* the screen, from the config: the stage's rectangle, the panes *)
let stage_left (c : config) = c.stage.cx -. (240. *. c.stage.k)
let stage_right (c : config) = c.stage.cx +. (240. *. c.stage.k)
let stage_top (c : config) = c.stage.cy +. (180. *. c.stage.k)
let stage_bottom (c : config) = c.stage.cy -. (180. *. c.stage.k)
let top_bar = 470.
let grid_top = 425.
let rows (c : config) = (List.length c.categories + 1) / 2
let blocks_top (c : config) = grid_top -. (float_of_int (rows c) *. 22.) +. 8.
let in_stage (c : config) (x, y) = x > stage_left c && x < stage_right c && y < stage_top c && y > stage_bottom c
let in_palette (c : config) (x, y) = x > fst c.palette_x && x < snd c.palette_x && y < top_bar
let in_scripts (c : config) (x, y) = x >= fst c.scripts_x && x < snd c.scripts_x && y < top_bar
let near (ax, ay) (bx, by) r = Float.hypot (ax -. bx) (ay -. by) < r
let rgb3 (r, g, b) = rgb (int_of_float r) (int_of_float g) (int_of_float b)

let column (c : config) text =
  let scripts = match Scratch_text.parse text with Ok s -> s | Error e -> failwith e in
  List.rev
    (fst
       (List.fold_left
          (fun (acc, y) (s : B.script) -> ({ s with x = fst c.scripts_x +. 15.; y } :: acc, y -. L.height ~measure s.blocks -. 30.))
          ([], 420.) scripts))

let sprite_of m = R.find m.stage m.current
let scripts_of m = (sprite_of m).scripts
let set_scripts m scripts = { m with stage = R.update m.stage { (sprite_of m) with scripts } }

(* the scripts where they are drawn, the scroll applied *)
let shown m = List.map (fun (s : B.script) -> { s with y = s.y +. m.scroll }) (scripts_of m)

(* Snap!'s "Make a block" button, over the custom blocks *)
let make_button m = if m.c.make_block && m.category = B.Other then Some (fst m.c.palette_x +. 60., blocks_top m.c +. m.palette_scroll -. 12.) else None

(* the palette's blocks, each where it is drawn *)
let palette m =
  let top = blocks_top m.c +. m.palette_scroll -. if make_button m <> None then 34. else 0. in
  List.rev
    (fst
       (List.fold_left
          (fun (acc, y) b ->
            let _, h = L.size ~measure b in
            ((b, (fst m.c.palette_x +. 10., y)) :: acc, y -. h -. 10.))
          ([], top) (m.c.palette m.stage m.category)))

let category_at (c : config) i = ((if i mod 2 = 0 then fst c.palette_x +. 60. else fst c.palette_x +. 168.), grid_top -. (float_of_int (i / 2) *. 22.))
let thumbnails m = List.mapi (fun i (s : R.sprite) -> (s.name, (stage_left m.c +. 50. +. (float_of_int i *. 95.), stage_bottom m.c -. 55.))) m.stage.sprites

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

let is_reporter (b : B.block) = match (B.spec b.op).shape with B.Reporter | B.Predicate | B.Ring | B.Command_ring -> true | _ -> false

(* where a dragged thing lands *)
let release m d =
  let x, y = d.at in
  let gx, gy = d.grab in
  let corner = (x -. gx, y -. gy) in
  if in_palette m.c (x, y) then m (* dropped on the palette: deleted *)
  else
    let scripts = shown m in
    let unscroll (s : B.script) = { s with y = s.y -. m.scroll } in
    let cx, cy = corner in
    let cx = Float.max (fst m.c.scripts_x +. 5.) cx in
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

(* a new custom block's definition, Snap!'s Block Editor as a script:
   its kind and its template typed in the hat's slots *)
let make_block m =
  let n = List.length (scripts_of m) in
  let def = { (B.make "procedures_definition") with args = [ B.Lit "command"; B.Lit (Printf.sprintf "block %d %%input" (n + 1)) ] } in
  (* at the top right, where the scripts are seldom *)
  { (set_scripts m (E.alone (scripts_of m) (snd m.c.scripts_x -. 240., 420. -. m.scroll) [ def ])) with category = B.Other }

let press_at m p =
  let c : config = m.c in
  let m = commit { m with said = None } in
  if near p c.flag 14. then { m with stage = R.green_flag m.stage }
  else if near p c.stop 14. then { m with stage = R.stop m.stage }
  else if in_stage c p then
    let sx, sy = Look.to_stage c.stage p in
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
              let cx, cy = category_at c i in
              Float.abs (x -. cx) < 52. && Float.abs (y -. cy) < 10.)
            (List.mapi (fun i cat -> (i, cat)) c.categories)
        in
        match (category, make_button m) with
        | Some (_, cat), _ -> { m with category = cat; palette_scroll = 0. }
        | None, Some (bx, by) when Float.abs (x -. bx) < 50. && Float.abs (y -. by) < 11. -> make_block m
        | None, _ ->
            if in_palette c p then
              match List.find_opt (fun (b, (bx, by)) -> let w, h = L.size ~measure b in x >= bx && x <= bx +. w && y <= by && y >= by -. h) (palette m) with
              | Some (b, (bx, by)) ->
                  let dragged = if is_reporter b then E.Reporter b else E.Stack [ b ] in
                  { m with drag = Some { dragged; grab = (x -. bx, y -. by); at = p } }
              | None -> m
            else if in_scripts c p then
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

let input m (computer : computer) keys =
  let mx, my = Look.to_stage m.c.stage (computer.mouse.mx, computer.mouse.my) in
  { R.mouse_x = mx; mouse_y = my; mouse_down = computer.mouse.mdown && in_stage m.c (computer.mouse.mx, computer.mouse.my); keys; time = float_of_int (m.frame / 2) /. 30. }

(* a click, not a drag: a script runs; a loose reporter says its value *)
let clicked m computer (path : L.path) =
  match List.nth_opt (scripts_of m) path.script with
  (* anywhere on it: the whole of it *)
  | Some { blocks = [ r ]; _ } when is_reporter r ->
      let v, stage = R.report (input m computer []) m.stage m.current r in
      { m with stage; said = Some (path.script, R.show stage v) }
  | _ -> { m with stage = R.run_script m.current path.script m.stage }

let step_stage m computer keys =
  (* Scratch 2's 30 frames a second, the Playground's 60 *)
  if m.frame mod 2 = 0 then { m with stage = R.step (input m computer keys) m.stage } else m

let update (computer : computer) m =
  let m =
    if m.started then m
    else
      let m = { m with started = true } in
      let m = match List.assoc_opt "sprite" computer.flags with Some name -> (match List.find_opt (fun (s : R.sprite) -> String.lowercase_ascii s.name = name) m.stage.sprites with Some s -> { m with current = s.name } | None -> m) | None -> m in
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
    else if in_palette m.c p then { m with palette_scroll = Float.max 0. (m.palette_scroll -. (mouse.mwheel *. 30.)) }
    else if in_scripts m.c p then { m with scroll = Float.max 0. (m.scroll -. (mouse.mwheel *. 30.)) }
    else m
  in
  let m =
    if mouse.mdown && not m.was_down then press_at m p
    else if mouse.mdown then match m.drag with Some d -> { m with drag = Some { d with at = p } } | None -> start_drag m p
    else if m.was_down then
      match (m.drag, m.press) with
      | Some d, _ -> release { m with drag = None } { d with at = p }
      | None, Some (path, _) -> clicked { m with press = None } computer path
      | None, None -> m
    else m
  in
  let m = step_stage m computer keys in
  { m with frame = m.frame + 1; was = now; was_down = mouse.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view (_ : computer) m =
  let c : config = m.c in
  let k = c.colors in
  let scripts = shown m in
  let pieces, _ = L.layout ~measure scripts in
  let palette_left, palette_right = c.palette_x and scripts_left, scripts_right = c.scripts_x in
  let stage_left = stage_left c and stage_right = stage_right c and stage_top = stage_top c and stage_bottom = stage_bottom c in
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
  (* a loose reporter's value, in a balloon over its right end, kept
     in the scripts area *)
  let said =
    match m.said with
    | Some (i, v) -> (
        match List.nth_opt scripts i with
        | Some sc ->
            let bw = measure v +. 16. in
            let x = Float.min (sc.x +. L.width ~measure sc.blocks -. bw) (snd c.scripts_x -. bw -. 8.) and y = sc.y +. 16. in
            [ rectangle (rgb 120 120 120) (bw +. 2.) 24. |> move (x +. (bw /. 2.)) y; rectangle white bw 22. |> move (x +. (bw /. 2.)) y; label black v (x +. 8., y) ]
        | None -> [])
    | None -> []
  in
  let caret = Option.map (fun (path, typed, _) -> (path, typed)) m.editing in
  let scripts_pane =
    [ rectangle k.scripts (scripts_right -. scripts_left) 1000. |> move ((scripts_left +. scripts_right) /. 2.) 0. ] @ glow @ Look.pieces_shapes c.theme ?caret pieces @ said
  in
  let palette_blocks = List.concat_map (fun ((b : B.block), (x, y)) -> Look.pieces_shapes c.theme (fst (L.layout ~measure [ { x; y; blocks = [ b ] } ]))) (palette m) in
  let button =
    match make_button m with
    | Some (x, y) -> [ rectangle k.line 100. 22. |> move x y; rectangle k.button_off 98. 20. |> move x y; text k.ink 12. "Make a block" (x, y) ]
    | None -> []
  in
  let categories =
    List.concat
      (List.mapi
         (fun i cat ->
           let cx, cy = category_at c i in
           let on = cat = m.category in
           let col = rgb3 (c.theme.colors cat) in
           [ rectangle (if on then col else k.button_off) 104. 19. |> move cx cy; rectangle col 6. 19. |> move (cx -. 49.) cy; text (if on then white else k.ink) 12. (B.category_name cat) (cx +. 3., cy) ])
         c.categories)
  in
  let palette_pane =
    [ rectangle k.pane (palette_right -. palette_left) 1000. |> move ((palette_left +. palette_right) /. 2.) 0. ]
    @ palette_blocks @ button
    @ [ rectangle k.header (palette_right -. palette_left) (top_bar -. (blocks_top c +. 10.)) |> move ((palette_left +. palette_right) /. 2.) ((top_bar +. blocks_top c +. 10.) /. 2.) ]
    @ categories
  in
  let s = sprite_of m in
  let thumbs =
    List.concat_map
      (fun (name, (x, y)) ->
        let sp = R.find m.stage name in
        let kk = 30. /. sp.radius in
        [ rectangle (if name = m.current then rgb 120 180 240 else k.line) 84. 84. |> move x y; rectangle k.sheet 80. 80. |> move x y ]
        @ [ group (Look.costume { sp with costume = 0 }) |> scale (kk *. 0.9) |> move x (y +. 6.); text k.ink 11. name (x, y -. 30.) ])
      (thumbnails m)
  in
  (* the text under the sprites, as tall as the space left *)
  let sheet_top = stage_bottom -. 135. in
  let sheet_h = sheet_top +. 440. in
  let source = String.split_on_char '\n' (Scratch_text.print (scripts_of m)) in
  let source = List.filteri (fun i _ -> i < int_of_float ((sheet_h -. 20.) /. 17.)) source in
  let outer = if c.stage.cx < 0. then -500. else 500. in
  let side =
    [ rectangle k.side (stage_right -. stage_left +. 10.) (stage_bottom +. 500.) |> move ((stage_left +. stage_right) /. 2.) ((stage_bottom -. 500.) /. 2.); rectangle k.header 10. 1000. |> move outer 0. ]
    @ [ rectangle k.header (stage_right -. stage_left +. 10.) (top_bar -. stage_top) |> move ((stage_left +. stage_right) /. 2.) ((top_bar +. stage_top) /. 2.) ]
    @ [ text k.ink 13. "Sprites" (stage_left +. 30., stage_bottom -. 7.) ]
    @ thumbs
    (* + 0. turns a -0, the cosine's, into 0 *)
    @ [ text k.ink 12. (Printf.sprintf "%s   x: %.0f  y: %.0f  direction: %.0f" s.name (Float.round s.x +. 0.) (Float.round s.y +. 0.) (Float.round s.direction +. 0.)) (c.stage.cx, stage_bottom -. 110.) ]
    @ [ rectangle k.sheet (stage_right -. stage_left -. 10.) sheet_h |> move c.stage.cx (sheet_top -. (sheet_h /. 2.)); text k.ink 12. "The same scripts as text (scratchblocks)" (c.stage.cx, stage_bottom -. 140.) ]
    @ List.mapi
        (fun i l ->
          (* the indentation as an offset, the text from its first word *)
          let t = String.trim l in
          let rec lead i = if i < String.length l && l.[i] = ' ' then lead (i + 1) else i in
          let indent = float_of_int (lead 0) in
          label k.sheet_ink t (stage_left +. 12. +. (indent *. 7.), sheet_top -. 25. -. (float_of_int i *. 17.)))
        source
  in
  let fx, fy = c.flag and sx, sy = c.stop in
  let buttons =
    [
      circle (if m.stage.threads <> [] then rgb 200 240 200 else white) 13. |> move fx fy;
      rectangle (rgb 60 60 60) 2. 18. |> move (fx -. 5.) fy;
      polygon (rgb 60 180 60) [ (fx -. 4., fy +. 8.); (fx +. 8., fy +. 4.); (fx -. 4., fy) ];
      circle white 13. |> move sx sy;
      octagon (rgb 220 50 50) 9. |> move sx sy;
    ]
  in
  let name = match c.name_at with Some xy -> [ text k.ink 13. m.current xy ] | None -> [] in
  let top = [ rectangle c.bar 1000. 30. |> move 0. 485.; text white 16. c.title (-440., 485.); text white 13. c.menus (-320., 485.) ] in
  let dragged =
    match m.drag with
    | Some { dragged; grab = gx, gy; at = x, y } ->
        let blocks = match dragged with E.Stack bs -> bs | E.Reporter r -> [ r ] in
        List.map (fade 0.85) (Look.pieces_shapes c.theme (fst (L.layout ~measure [ { x = x -. gx; y = y -. gy; blocks } ])))
    | None -> []
  in
  (* the title bar before the buttons: Snap!'s are on it *)
  Look.stage_shapes c.stage m.stage @ scripts_pane @ palette_pane @ side @ top @ buttons @ name @ dragged

let app c stage ~current =
  let initial =
    {
      c;
      stage;
      current;
      category = B.Motion;
      palette_scroll = 0.;
      scroll = 0.;
      press = None;
      drag = None;
      editing = None;
      said = None;
      frame = 0;
      started = false;
      was = [];
      was_down = false;
    }
  in
  game view update initial
