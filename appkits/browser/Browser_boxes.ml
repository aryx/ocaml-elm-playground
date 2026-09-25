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

(* a rectangle of the page (y down), [w] by [h] from (x, y) *)
let fill (c : color) (x : float) (y : float) (w : float) (h : float) : shape =
  rectangle c w h |> move (x +. (w /. 2.)) (-.(y +. (h /. 2.)))

let rec draw ~(visited : string -> bool) ~(picture_of : string -> Browser_picture.t option) (b : Box_layout.box) : Browser_draw.drawn =
  let s = b.style in
  let own =
    if b.element = None || not s.visible then []
    else
      let background = match color s.background with Some c -> [ fill c b.x b.y b.width b.height ] | None -> [] in
      let bt, br, bb, bl = b.border and ct, cr, cb, cl = s.border_color in
      let side width c shape = if width > 0. then match color c with Some c -> [ shape c ] | None -> [] else [] in
      let borders =
        side bt ct (fun c -> fill c b.x b.y b.width bt)
        @ side bb cb (fun c -> fill c b.x (b.y +. b.height -. bb) b.width bb)
        @ side bl cl (fun c -> fill c b.x b.y bl b.height)
        @ side br cr (fun c -> fill c (b.x +. b.width -. br) b.y br b.height)
      in
      match background @ borders with [] -> [] | shapes -> [ (b.y, b.y +. b.height, group shapes) ]
  in
  let lines =
    List.map
      (fun (l : Html_layout.line) ->
        (l.top, l.top +. l.height, group (List.concat_map (Browser_draw.glyphs ~visited ~picture_of) l.fragments)))
      b.lines
  in
  (* a list item's marker, left of its first line, in the list's colour *)
  let marker =
    let first = List.find_map (fun (c : Box_layout.box) -> match c.lines with l :: _ -> Some l | [] -> None) b.children in
    match (b.marker, first) with
    | Some m, Some line ->
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
  own @ lines @ marker @ List.concat_map (draw ~visited ~picture_of) b.children
