(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_devtools.mli *)

type color = int * int * int
type line = string * color

(* Chrome's inspector's colours: tags purple, attributes brown, values
 * blue, the rest grey *)
let ink = (32, 33, 36)
let tag_color = (136, 18, 128)
let value_color = (26, 26, 166)
let muted = (110, 110, 110)
let red = (200, 30, 30)

(* "div#main.card": an element as a selector names it *)
let label (e : Dom.element) : string =
  e.name
  ^ (match Dom.attribute "id" e with Some i -> "#" ^ i | None -> "")
  ^ match Dom.attribute "class" e with Some c -> String.concat "" (List.map (fun w -> if w = "" then "" else "." ^ w) (String.split_on_char ' ' c)) | None -> ""

let rec path_to (root : Dom.element) (e : Dom.element) : Dom.element list option =
  if root == e then Some [ e ]
  else List.find_map (fun (n : Dom.node) -> match n with Element c -> Option.map (fun p -> root :: p) (path_to c e) | Text _ -> None) root.children

let short (n : int) (s : string) : string = if String.length s <= n then s else String.sub s 0 (max 0 (n - 3)) ^ "..."

let box_of (p : Browser_page.t) (e : Dom.element) : (float * float * float * float) option =
  let rec find (b : Html_layout.box) =
    match b.kind with Block e' when e' == e -> Some (b.x, b.y, b.width, b.height) | _ -> List.find_map find b.children
  in
  match find p.layout with
  | Some _ as found -> found
  | None -> (
      (* an inline element: around its words *)
      let rec inside (x : Dom.element) = x == e || List.exists (fun (n : Dom.node) -> match n with Element c -> inside c | Text _ -> false) x.children in
      match List.filter (fun (f : Html_layout.fragment) -> inside f.element || f.element == e) (Html_layout.fragments p.layout) with
      | [] -> None
      | fs ->
          let x0 = List.fold_left (fun m (f : Html_layout.fragment) -> Float.min m f.x) infinity fs in
          let x1 = List.fold_left (fun m (f : Html_layout.fragment) -> Float.max m (f.x +. f.width)) neg_infinity fs in
          let y0 = List.fold_left (fun m (f : Html_layout.fragment) -> Float.min m (f.baseline -. (0.8 *. f.look.size))) infinity fs in
          let y1 = List.fold_left (fun m (f : Html_layout.fragment) -> Float.max m (f.baseline +. (0.2 *. f.look.size))) neg_infinity fs in
          Some (x0, y0, x1 -. x0, y1 -. y0))

let element (p : Browser_page.t) (e : Dom.element) : line list =
  let path = match path_to p.tree e with Some l -> String.concat " > " (List.map label l) | None -> label e in
  let attributes = String.concat "" (List.map (fun (k, v) -> Printf.sprintf " %s=\"%s\"" k (short 40 v)) e.attributes) in
  let children =
    List.filter_map
      (fun (n : Dom.node) ->
        match n with
        | Element c -> Some ("  <" ^ label c ^ ">", tag_color)
        | Text t ->
            let t = String.trim (String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) t) in
            if t = "" then None else Some ("  \"" ^ short 70 t ^ "\"", ink))
      e.children
  in
  let n = List.length children in
  [ (short 110 path, muted); (short 110 ("<" ^ e.name ^ attributes ^ ">"), tag_color) ]
  @ (match box_of p e with
    | Some (x, y, w, h) -> [ (Printf.sprintf "box: x %.0f, y %.0f, %.0f x %.0f" x y w h, value_color) ]
    | None -> [ ("not laid out (display: none?)", muted) ])
  @ List.filteri (fun i _ -> i < 12) children
  @ if n > 12 then [ (Printf.sprintf "  ... %d more" (n - 12), muted) ] else []

let styles (s : Browser_page.settings) (p : Browser_page.t) (e : Dom.element) : line list =
  match Browser_page.explain s p e with
  | [] -> [ ("no declarations: all inherited or initial", muted) ]
  | decls ->
      List.concat_map
        (fun (prop, value, where) -> [ (Printf.sprintf "%s: %s;" prop (short 60 value), value_color); ("    " ^ short 100 where, muted) ])
        decls

let kind_name (k : Browser_tab.kind) : string = match k with Document -> "page" | Sheet -> "css" | Picture -> "img"

let network (requests : Browser_tab.request list) ~(times : string -> (float * float option) option) : line list =
  let rs = List.rev requests in
  let total = List.fold_left (fun t (r : Browser_tab.request) -> t + r.bytes) 0 rs in
  let pending = List.length (List.filter (fun (r : Browser_tab.request) -> r.status = None) rs) in
  let finish = List.fold_left (fun m (r : Browser_tab.request) -> match times r.url with Some (_, Some t) -> Float.max m t | _ -> m) neg_infinity rs in
  let start = List.fold_left (fun m (r : Browser_tab.request) -> match times r.url with Some (t, _) -> Float.min m t | _ -> m) infinity rs in
  let summary =
    Printf.sprintf "%d requests, %.1f KB%s%s" (List.length rs) (float_of_int total /. 1024.)
      (if finish > start then Printf.sprintf ", %.2f s" (finish -. start) else "")
      (if pending > 0 then Printf.sprintf ", %d waiting" pending else "")
  in
  (summary, ink)
  :: ("status  kind  size      time    address", muted)
  :: List.map
       (fun (r : Browser_tab.request) ->
         let status = match r.status with None -> "..." | Some 0 -> "fail" | Some n -> string_of_int n in
         let time = match times r.url with Some (a, Some b) -> Printf.sprintf "%4.0f ms" ((b -. a) *. 1000.) | Some (_, None) -> "  ..." | None -> "" in
         let size = if r.bytes >= 1024 then Printf.sprintf "%.1f KB" (float_of_int r.bytes /. 1024.) else Printf.sprintf "%d B" r.bytes in
         ( Printf.sprintf "%-7s %-5s %-9s %-7s %s" status (kind_name r.kind) size time (short 80 r.url),
           match r.status with Some n when n = 0 || n >= 400 -> red | _ -> ink ))
       rs
