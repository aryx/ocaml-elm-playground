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
      let background = match color s.background with Some c -> fill c b.x b.y b.width b.height | None -> [] in
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
          Some (l.top, l.top +. l.height, group (List.concat_map (Browser_draw.glyphs ~visited ~picture_of) shown)))
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
