(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_boxes.mli *)
open Playground

let color (c : Css_values.color) : color option =
  if c.a <= 0. then None
  else
    let mix v = int_of_float ((float_of_int v *. c.a) +. (255. *. (1. -. c.a))) in
    Some (rgb (mix c.r) (mix c.g) (mix c.b))

(* the rectangle what is drawn must stay in (left, top, right,
 * bottom): a box with overflow other than visible clips what it holds
 * -- here by what is drawn, not by pixels: a rectangle is cut to it, a
 * line or a word not wholly inside is left out *)
type clip = float * float * float * float

let everywhere : clip = (neg_infinity, neg_infinity, infinity, infinity)
let meet ((l, t, r, b) : clip) ((l', t', r', b') : clip) : clip = (Float.max l l', Float.max t t', Float.min r r', Float.min b b')
let inside ((l, t, r, b) : clip) (x : float) (y : float) (w : float) (h : float) : bool =
  x >= l -. 0.5 && y >= t -. 0.5 && x +. w <= r +. 0.5 && y +. h <= b +. 0.5

(*****************************************************************************)
(* Inline SVG *)
(*****************************************************************************)

(* an <svg> of the page as Svg's nodes: its elements, its text dropped *)
let rec svg_node (e : Dom.element) : Svg.node =
  { name = e.name; attributes = e.attributes @ e.extensions;
    children = List.filter_map (fun (n : Dom.node) -> match n with Element c -> Some (svg_node c) | Text _ -> None) e.children }

(* its pixels, by element (==), colour and size: a page laid out again
 * as each of its pictures arrives draws the same icons again (GitHub's
 * hundreds) *)
let rendered : (int, Dom.element * (int * int * int) * int * int * Rgba_image.t) Hashtbl.t = Hashtbl.create 64

let svg_picture (e : Dom.element) (color : int * int * int) (w : int) (h : int) : Rgba_image.t =
  let key = Hashtbl.hash (w, h, color, e.name, e.attributes) in
  match List.find_opt (fun (e', c, w', h', _) -> e' == e && c = color && w' = w && h' = h) (Hashtbl.find_all rendered key) with
  | Some (_, _, _, _, img) -> img
  | None ->
      if Hashtbl.length rendered > 4096 then Hashtbl.reset rendered;
      let img = Svg.render ~color (svg_node e) ~width:w ~height:h in
      Hashtbl.add rendered key (e, color, w, h, img);
      img

(* a fragment's shapes: an inline <svg>'s picture drawn here, the rest
 * by Browser_draw *)
let glyphs ~visited ~picture_of (f : Html_layout.fragment) : shape list =
  match f.picture with
  (* a player's place: black, until the browser draws what plays there
   * (a video's frame, the controls) over it *)
  | Some { src = ""; height; middle } when (f.element.name = "video" || f.element.name = "audio") && f.width >= 1. ->
      let centre = if middle then f.baseline else f.baseline -. (height /. 2.) in
      [ rectangle (if f.element.name = "video" then rgb 0 0 0 else rgb 241 243 244) f.width height |> move (f.x +. (f.width /. 2.)) (-.centre) ]
  | Some { src = ""; height; middle } when f.element.name = "svg" && f.width >= 1. && height >= 1. ->
      let img = svg_picture f.element f.look.color (int_of_float (Float.round f.width)) (int_of_float (Float.round height)) in
      (* its bottom on the baseline, or its middle *)
      let centre = if middle then f.baseline else f.baseline -. (height /. 2.) in
      [ bitmap f.width height img |> move (f.x +. (f.width /. 2.)) (-.centre) ]
  (* a picture in a link without Mosaic's frame of the link's colour:
   * here borders are the style sheets' *)
  | Some _ -> Browser_draw.glyphs ~visited ~picture_of { f with look = { f.look with link = None } }
  | None -> Browser_draw.glyphs ~visited ~picture_of f

(* a picture in one colour, its alpha kept: a mask's shape in the
 * background's colour -- by picture (==) and colour *)
let tinted : (Rgba_image.t * (int * int * int) * Rgba_image.t) list ref = ref []

let tint (img : Rgba_image.t) ((r, g, b) : int * int * int) : Rgba_image.t =
  match List.find_opt (fun (i, c, _) -> i == img && c = (r, g, b)) !tinted with
  | Some (_, _, t) -> t
  | None ->
      let t = Rgba_image.create ~width:img.width ~height:img.height in
      for i = 0 to (img.width * img.height) - 1 do
        t.rgba.{4 * i} <- r;
        t.rgba.{(4 * i) + 1} <- g;
        t.rgba.{(4 * i) + 2} <- b;
        t.rgba.{(4 * i) + 3} <- img.rgba.{(4 * i) + 3}
      done;
      if List.length !tinted > 256 then tinted := [];
      tinted := (img, (r, g, b), t) :: !tinted;
      t

(*****************************************************************************)
(* Boxes *)
(*****************************************************************************)

(* a rectangle of the page (y down), [w] by [h] from (x, y), cut to
 * [clip] *)
let fill ?(clip = everywhere) (c : color) (x : float) (y : float) (w : float) (h : float) : shape list =
  let l, t, r, b = meet clip (x, y, x +. w, y +. h) in
  if r <= l || b <= t then [] else [ rectangle c (r -. l) (b -. t) |> move ((l +. r) /. 2.) (-.((t +. b) /. 2.)) ]

let rec draw_in (clip : clip) ~(visited : string -> bool) ~(picture_of : string -> Browser_picture.t option) (b : Box_layout.box) :
    Browser_draw.drawn =
  let s = b.style in
  let own =
    if b.element = None || not s.visible then []
    else
      let fill = fill ~clip in
      (* a mask: the background's colour through the picture's shape
       * only, the picture tinted with it (its alpha kept); nothing
       * until it has come *)
      let background =
        match (s.mask_image, color s.background) with
        | Some u, Some _ -> (
            match picture_of u with
            | Some (Arrived img) when img.width > 0 ->
                let tinted = tint img (s.background.r, s.background.g, s.background.b) in
                let k = Float.min (b.width /. float_of_int img.width) (b.height /. float_of_int img.height) in
                let w = float_of_int img.width *. k and h = float_of_int img.height *. k in
                if w >= 1. && inside clip b.x b.y b.width b.height then
                  [ bitmap w h tinted |> move (b.x +. (b.width /. 2.)) (-.(b.y +. (b.height /. 2.))) ]
                else []
            | _ -> [])
        | _, Some c -> fill c b.x b.y b.width b.height
        | _, None -> []
      in
      (* its picture, once it has come: at its own size at the top left,
       * or shrunk to fit if it is larger (HN's vote arrows: an SVG
       * drawn in a 10 by 10 box) -- background-size and -position
       * read as that; drawn if wholly inside what clips *)
      let background =
        background
        @
        match Option.map picture_of s.background_image with
        | Some (Some (Arrived img)) when img.width > 0 && img.height > 0 ->
            let iw = float_of_int img.width and ih = float_of_int img.height in
            let k = Float.min 1. (Float.min (b.width /. iw) (b.height /. ih)) in
            let w = iw *. k and h = ih *. k in
            if w >= 1. && h >= 1. && inside clip b.x b.y w h then [ bitmap w h img |> move (b.x +. (w /. 2.)) (-.(b.y +. (h /. 2.))) ] else []
        | _ -> []
      in
      let bt, br, bb, bl = b.border and ct, cr, cb, cl = s.border_color in
      let side width c shape = if width > 0. then match color c with Some c -> shape c | None -> [] else [] in
      let borders =
        side bt ct (fun c -> fill c b.x b.y b.width bt)
        @ side bb cb (fun c -> fill c b.x (b.y +. b.height -. bb) b.width bb)
        @ side bl cl (fun c -> fill c b.x b.y bl b.height)
        @ side br cr (fun c -> fill c (b.x +. b.width -. br) b.y br b.height)
      in
      match background @ borders with [] -> [] | shapes -> [ (b.y, b.y +. b.height, group shapes) ]
  in
  let lines =
    List.filter_map
      (fun (l : Html_layout.line) ->
        let _, t, _, bottom = clip in
        if l.top < t -. 0.5 || l.top +. l.height > bottom +. 0.5 then None
        else
          let shown = List.filter (fun (f : Html_layout.fragment) -> inside clip f.x l.top f.width 0.) l.fragments in
          Some (l.top, l.top +. l.height, group (List.concat_map (glyphs ~visited ~picture_of) shown)))
      b.lines
  in
  (* a list item's marker, left of its first line, in the list's colour *)
  let marker =
    let first = List.find_map (fun (c : Box_layout.box) -> match c.lines with l :: _ -> Some l | [] -> None) b.children in
    match (b.marker, first) with
    | Some m, Some line when inside clip b.x line.top 0. line.height ->
        let look = Box_layout.look_of s ~link:None in
        let text = match m with Bullet -> "\xe2\x80\xa2" | Number n -> string_of_int n ^ "." in
        let ink = rgb s.color.r s.color.g s.color.b in
        let shape =
          match m with
          | Bullet -> circle ink (0.17 *. s.font_size) |> move (b.x -. (0.8 *. s.font_size)) (-.(line.baseline -. (0.3 *. s.font_size)))
          | Number _ ->
              let width = Browser_text.metrics look text in
              group
                (Browser_draw.glyphs
                   { text; look; x = b.x -. (0.4 *. s.font_size) -. width; width; baseline = line.baseline; picture = None; control = None;
                     element = Dom.element "li" [] })
        in
        [ (line.top, line.top +. line.height, shape) ]
    | _ -> []
  in
  (* what it holds cut to it, if it clips *)
  let inner = if b.element <> None && s.overflow_hidden then meet clip (b.x, b.y, b.x +. b.width, b.y +. b.height) else clip in
  (* its inline elements' boxes under its words *)
  own
  @ List.concat_map (draw_in clip ~visited ~picture_of) b.backdrops
  @ lines @ marker
  @ List.concat_map (draw_in inner ~visited ~picture_of) b.children

let draw ~visited ~picture_of (b : Box_layout.box) : Browser_draw.drawn = draw_in everywhere ~visited ~picture_of b
