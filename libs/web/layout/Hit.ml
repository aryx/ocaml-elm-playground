(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Hit.mli *)

(* the link on a line at [x]: in a fragment, or in the space between
 * two fragments of the same link *)
let rec on_line (fragments : Html_layout.fragment list) (x : float) : string option =
  match fragments with
  | [] -> None
  | f :: rest ->
      if x >= f.x && x <= f.x +. f.width then f.look.link
      else (
        match rest with
        | next :: _ when x > f.x +. f.width && x < next.x && f.look.link <> None && f.look.link = next.look.link ->
            f.look.link
        | _ -> on_line rest x)

let rec link_at (b : Html_layout.box) ~(x : float) ~(y : float) : string option =
  if y < b.y || y > b.y +. b.height then None
  else
    let in_lines =
      List.find_map
        (fun (l : Html_layout.line) -> if y >= l.top && y <= l.top +. l.height then on_line l.fragments x else None)
        b.lines
    in
    match in_lines with Some _ -> in_lines | None -> List.find_map (fun c -> link_at c ~x ~y) b.children

let rec anchor (b : Html_layout.box) (name : string) : float option =
  match b.kind with
  | Block e when Dom.attribute "id" e = Some name -> Some b.y
  | _ -> (
      match List.find_opt (fun (l : Html_layout.line) -> List.mem name l.anchors) b.lines with
      | Some l -> Some l.top
      | None -> List.find_map (fun c -> anchor c name) b.children)
