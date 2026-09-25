(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_draw.mli *)
open Playground

type drawn = (float * float * shape) list

let characters = Browser_text.characters
let metrics = Browser_text.metrics

let frame ?(t = 1.) (color : color) (x : float) (y : float) (w : float) (h : float) : shape =
  group
    [ rectangle color w t |> move (x +. (w /. 2.)) (-.(y +. (t /. 2.)));
      rectangle color w t |> move (x +. (w /. 2.)) (-.(y +. h -. (t /. 2.)));
      rectangle color t h |> move (x +. (t /. 2.)) (-.(y +. (h /. 2.)));
      rectangle color t h |> move (x +. w -. (t /. 2.)) (-.(y +. (h /. 2.))) ]

let raised (x : float) (top : float) (w : float) (h : float) : shape list =
  [ rectangle (rgb 205 205 205) w h |> move (x +. (w /. 2.)) (-.(top +. (h /. 2.)));
    frame (rgb 120 120 120) x top w h;
    frame (rgb 240 240 240) x top (w -. 1.) (h -. 1.) ]

let sunken (x : float) (top : float) (w : float) (h : float) : shape list =
  [ rectangle (rgb 255 255 255) w h |> move (x +. (w /. 2.)) (-.(top +. (h /. 2.)));
    frame (rgb 240 240 240) x top w h;
    frame (rgb 120 120 120) x top (w -. 1.) (h -. 1.) ]

(*****************************************************************************)
(* Words and pictures *)
(*****************************************************************************)

(* a picture in its place: its pixels, once arrived; the room kept for
 * it (its width= and height=) until then; NCSA's broken image when it
 * could not be had; in a link, a border of the link's colour, as
 * Mosaic drew it (the one way to tell a picture that is a link) *)
let picture_shapes (state : Browser_picture.t option) (color : color) (f : Html_layout.fragment) (pic : Html_layout.picture)
    : shape list =
  let w = f.width and h = pic.height in
  let top = if pic.middle then f.baseline -. (h /. 2.) else f.baseline -. h in
  let center = (f.x +. (w /. 2.), -.(top +. (h /. 2.))) in
  let body =
    match state with
    | Some (Arrived img) -> [ bitmap w h img |> move (fst center) (snd center) ]
    | Some Broken ->
        (* a torn picture: a white card, a red slash across *)
        [ rectangle (rgb 245 245 245) w h |> move (fst center) (snd center);
          frame (rgb 90 90 90) f.x top w h;
          rectangle (rgb 200 30 30) (w *. 1.2) 2. |> rotate 45. |> move (fst center) (snd center) ]
    | Some Waiting | None ->
        (* the room kept: a sunken, empty frame *)
        [ frame (rgb 130 130 130) f.x top w h; frame (rgb 235 235 235) (f.x +. 1.) (top +. 1.) (w -. 2.) (h -. 2.) ]
  in
  body @ match f.look.link with Some _ -> [ frame ~t:2. color (f.x -. 2.) (top -. 2.) (w +. 4.) (h +. 4.) ] | None -> []

let glyphs ?(visited = fun (_ : string) -> false) ?(picture_of = fun (_ : string) -> None) (f : Html_layout.fragment) :
    shape list =
  let style = Browser_text.style_of f.look in
  (* a visited link's colour: Mosaic's purple, and every browser's
   * since (or the page's vlink=) *)
  let (r, g, b) = match f.look.link with Some href when visited href -> f.look.visited_color | _ -> f.look.color in
  let color = rgb r g b in
  let baseline = -.f.baseline in
  match (f.picture, f.control) with
  | Some pic, _ -> picture_shapes (picture_of pic.src) color f pic
  (* a form's control: drawn with its value every frame (control_shapes) *)
  | None, Some _ -> []
  | None, None ->
      if f.look.monospace then
        let cell = Browser_text.cell_of f.look in
        characters f.text
        |> List.mapi (fun i c ->
               if c = " " then []
               else
                 let w = Stroke_text.metrics style c in
                 Stroke_text.glyph color style c ~x:(f.x +. (cell *. float_of_int i) +. ((cell -. w) /. 2.)) ~baseline)
        |> List.concat
      else
        let _, shapes =
          List.fold_left
            (fun (x, shapes) c -> (x +. Stroke_text.metrics style c, Stroke_text.glyph color style c ~x ~baseline :: shapes))
            (f.x, []) (characters f.text)
        in
        List.concat (List.rev shapes)

(* a bevelled frame [t] thick: [light] above and on the left, [dark]
 * below and on the right -- raised; the other way round, sunken *)
let bevel ~(light : color) ~(dark : color) ~(t : float) (x : float) (y : float) (w : float) (h : float) : shape list =
  [ rectangle light w t |> move (x +. (w /. 2.)) (-.(y +. (t /. 2.)));
    rectangle light t h |> move (x +. (t /. 2.)) (-.(y +. (h /. 2.)));
    rectangle dark w t |> move (x +. (w /. 2.)) (-.(y +. h -. (t /. 2.)));
    rectangle dark t h |> move (x +. w -. (t /. 2.)) (-.(y +. (h /. 2.))) ]

(* a table's border (Netscape 1.1's <table border=n>): the table raised
 * by n, each cell sunken by 1 -- Motif's look, the one the first
 * tables had *)
let table_frame (table : Dom.element) (b : Html_layout.box) : drawn =
  let t =
    match Dom.attribute "border" table with
    | Some "" -> 1.
    | Some s -> Option.value (float_of_string_opt s) ~default:1.
    | None -> 0.
  in
  if t <= 0. then []
  else
    let light = rgb 240 240 240 and dark = rgb 110 110 110 in
    (* below its caption *)
    let top = match b.children with { kind = Block c; y; height; _ } :: _ when c.name = "caption" -> y +. height | _ -> b.y in
    let cells =
      List.concat_map
        (fun (c : Html_layout.box) ->
          match c.kind with
          | Block e when e.name = "td" || e.name = "th" -> bevel ~light:dark ~dark:light ~t:1. c.x c.y c.width c.height
          | _ -> [])
        b.children
    in
    [ (top, b.y +. b.height, group (bevel ~light ~dark ~t b.x top b.width (b.y +. b.height -. top) @ cells)) ]

let rec draw ?(extensions = false) ~(visited : string -> bool) ~(picture_of : string -> Browser_picture.t option)
    (b : Html_layout.box) : drawn =
  let lines =
    List.map
      (fun (l : Html_layout.line) ->
        (l.top, l.top +. l.height, group (List.concat_map (glyphs ~visited ~picture_of) l.fragments)))
      b.lines
  in
  (* a box's floats, each drawn over its own height, not its line's *)
  let floats =
    List.filter_map
      (fun (f : Html_layout.fragment) ->
        match f.picture with
        | Some pic -> Some (f.baseline -. pic.height, f.baseline, group (glyphs ~visited ~picture_of f))
        | None -> None)
      b.floats
  in
  let rule =
    match b.kind with
    | Rule e when extensions && Dom.attribute ~extensions "noshade" e <> None ->
        (* Netscape's noshade: a flat grey bar *)
        [ (b.y, b.y +. b.height, rectangle (rgb 110 110 110) b.width b.height |> move (b.x +. (b.width /. 2.)) (-.b.y -. (b.height /. 2.))) ]
    | Rule _ ->
        (* an inset line, as Mosaic's Motif drew it: dark above, light
         * below; its sides too when Netscape's size= makes it thick *)
        let h = b.height in
        [ ( b.y,
            b.y +. h,
            group
              ([ rectangle (rgb 130 130 130) b.width 1. |> move (b.x +. (b.width /. 2.)) (-.b.y -. 0.5);
                 rectangle (rgb 235 235 235) b.width 1. |> move (b.x +. (b.width /. 2.)) (-.b.y -. h +. 0.5) ]
              @
              if h > 2. then
                [ rectangle (rgb 130 130 130) 1. h |> move (b.x +. 0.5) (-.b.y -. (h /. 2.));
                  rectangle (rgb 235 235 235) 1. h |> move (b.x +. b.width -. 0.5) (-.b.y -. (h /. 2.)) ]
              else []) ) ]
    | _ -> []
  in
  (* a list item's marker, left of its first line, in its list's indent *)
  let marker =
    match (b.marker, Html_layout.first_baseline b) with
    | Some Bullet, Some baseline ->
        [ (baseline -. 12., baseline, circle (rgb 0 0 0) 3. |> move (b.x -. 12.) (-.(baseline -. 5.))) ]
    | Some (Number n), Some baseline ->
        let text = string_of_int n ^ "." in
        let look = Browser_text.root_look in
        let width = metrics look text in
        [ ( baseline -. 12.,
            baseline,
            group (glyphs { text; look; x = b.x -. 6. -. width; width; baseline; picture = None; control = None }) ) ]
    | _ -> []
  in
  let frame = match b.kind with Block e when extensions && e.name = "table" -> table_frame e b | _ -> [] in
  (* a style sheet's background-color: under the box's content *)
  let background =
    match b.background with
    | Some (r, g, bl) ->
        [ (b.y, b.y +. b.height, rectangle (rgb r g bl) b.width b.height |> move (b.x +. (b.width /. 2.)) (-.(b.y +. (b.height /. 2.)))) ]
    | None -> []
  in
  background @ lines @ floats @ rule @ marker @ frame @ List.concat_map (draw ~extensions ~visited ~picture_of) b.children

(*****************************************************************************)
(* Form controls *)
(*****************************************************************************)

(* text in a control: black, plain, fixed-width if [cells] *)
let text_shapes ?(cells = false) (look : Looks.t) (text : string) ~(x : float) ~(baseline : float) : shape list =
  let look = { look with color = (0, 0, 0); underline = false; link = None; bold = false; italic = false; monospace = cells } in
  glyphs { text; look; x; width = metrics look text; baseline; picture = None; control = None }

let control_shapes ~(value : Dom.element -> Forms.value) ~(focused : bool) (f : Html_layout.fragment) (c : Html_layout.control)
    : shape list =
  match Forms.control c.element with
  | None -> []
  | Some control -> (
      let v = value c.element in
      let w = f.width and h = c.control_height and size = f.look.size in
      let top = f.baseline -. (0.75 *. h) and cell = Browser_text.cell_of f.look in
      let tail = Browser_text.tail in
      let caret x baseline = if focused then [ rectangle (rgb 0 0 0) 1.5 size |> move x (-.(baseline -. (0.35 *. size))) ] else [] in
      match control.kind with
      | Text | Password ->
          let shown = if control.kind = Password then String.make (List.length (characters v.text)) '*' else v.text in
          let shown = tail (int_of_float ((w -. 8.) /. cell) - 1) shown in
          sunken f.x top w h
          @ text_shapes ~cells:true f.look shown ~x:(f.x +. 4.) ~baseline:f.baseline
          @ caret (f.x +. 4. +. (cell *. float_of_int (List.length (characters shown)))) f.baseline
      | Checkbox ->
          (* Motif's toggle: a square, sunken and filled when on *)
          if v.checked then
            sunken f.x top w h
            @ [ rectangle (rgb 60 60 60) (w *. 0.5) (h *. 0.5) |> move (f.x +. (w /. 2.)) (-.(top +. (h /. 2.))) ]
          else raised f.x top w h
      | Radio ->
          (* Motif's radio button: a diamond, filled when on *)
          let center = (f.x +. (w /. 2.), -.(top +. (h /. 2.))) in
          let diamond color side = rectangle color side side |> rotate 45. |> move (fst center) (snd center) in
          [ diamond (rgb 120 120 120) (w *. 0.72); diamond (if v.checked then rgb 60 60 60 else rgb 225 225 225) (w *. 0.5) ]
      | Submit | Reset ->
          let label = Forms.label control in
          raised f.x top w h @ text_shapes f.look label ~x:(f.x +. ((w -. metrics f.look label) /. 2.)) ~baseline:f.baseline
      | Select opts ->
          (* Motif's option menu: the choice, and its little bar *)
          let label = match List.nth_opt opts v.selected with Some (l, _) -> l | None -> "" in
          raised f.x top w h
          @ text_shapes f.look label ~x:(f.x +. (0.4 *. size)) ~baseline:f.baseline
          @ raised (f.x +. w -. (1.3 *. size)) (top +. (h /. 2.) -. (0.2 *. size)) (0.9 *. size) (0.4 *. size)
      | Textarea ->
          let rows = max 1 (int_of_float ((h -. 8.) /. (Looks.leading *. size))) in
          let lines = String.split_on_char '\n' v.text in
          let n = List.length lines in
          (* the last rows when typing into it, else the first *)
          let shown = List.filteri (fun i _ -> if focused then i >= n - rows else i < rows) lines in
          let baseline i = top +. 4. +. (0.95 *. size) +. (float_of_int i *. Looks.leading *. size) in
          let columns = int_of_float ((w -. 8.) /. cell) in
          sunken f.x top w h
          @ List.concat
              (List.mapi (fun i line -> text_shapes ~cells:true f.look (tail columns line) ~x:(f.x +. 4.) ~baseline:(baseline i)) shown)
          @ (match List.rev shown with
            | last :: _ ->
                caret (f.x +. 4. +. (cell *. float_of_int (List.length (characters (tail columns last))))) (baseline (List.length shown - 1))
            | [] -> [])
      | Hidden -> [])

let controls_drawn ~(value : Dom.element -> Forms.value) ~(focus : Dom.element option) (layout : Html_layout.box) : drawn =
  Html_layout.fragments layout
  |> List.filter_map (fun (f : Html_layout.fragment) ->
         match f.control with
         | Some c ->
             let focused = match focus with Some e -> e == c.element | None -> false in
             Some (f.baseline -. c.control_height, f.baseline +. c.control_height, group (control_shapes ~value ~focused f c))
         | None -> None)

(*****************************************************************************)
(* The inspector *)
(*****************************************************************************)

let rec outlines (b : Html_layout.box) : drawn =
  let color = match b.kind with Anonymous -> rgb 0 150 0 | _ -> rgb 0 0 220 in
  ((b.y, b.y +. b.height, frame color b.x b.y b.width b.height)
  :: List.map (fun (l : Html_layout.line) -> (l.top, l.top +. l.height, frame (rgb 150 150 150) b.x l.top b.width l.height)) b.lines)
  @ List.concat_map outlines b.children
