(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Diagram.mli *)

type kind = Box | Connector
type ends = Begin | End

type master = {
  name : string;
  kind : kind;
  cells : (string * string) list;
  geometry : (string * string) list;
  points : (string * string * Ortho_route.dir) list;
}

type shape = {
  id : int;
  master : string;
  kind : kind;
  rows : (string * string) list;
  geometry : (string * string) list;
  points : (string * string * Ortho_route.dir) list;
  text : string;
  glue : (ends * int * int) list;
}

type t = { sheet : Sheet.t; shapes : shape list; next : int }

let empty = { sheet = Sheet.empty; shapes = []; next = 1 }
let shapes t = t.shapes
let shape t id = List.find_opt (fun s -> s.id = id) t.shapes

(*****************************************************************************)
(* The stencil *)
(*****************************************************************************)

(* a box's master: its size, cells of its own (User.), its outline as
   formulas, and a connection point in the middle of each side *)
let box name ?(user = []) (outline : (string * string) list) : master =
  let geometry = List.mapi (fun i _ -> (Printf.sprintf "Geometry1.X%d" (i + 1), Printf.sprintf "Geometry1.Y%d" (i + 1))) outline in
  let sides = [ ("Width*0.5", "Height", Ortho_route.Up); ("Width", "Height*0.5", Ortho_route.Right); ("Width*0.5", "0", Ortho_route.Down); ("0", "Height*0.5", Ortho_route.Left) ] in
  let points = List.mapi (fun i (_, _, d) -> (Printf.sprintf "Connections.X%d" (i + 1), Printf.sprintf "Connections.Y%d" (i + 1), d)) sides in
  let cells =
    [ ("Width", "1.5"); ("Height", "0.75") ]
    @ user
    @ List.concat (List.map2 (fun (xn, yn) (x, y) -> [ (xn, x); (yn, y) ]) geometry outline)
    @ List.concat (List.map2 (fun (xn, yn, _) (x, y, _) -> [ (xn, x); (yn, y) ]) points sides)
  in
  { name; kind = Box; cells; geometry; points }

let masters =
  [
    box "Process" [ ("0", "0"); ("Width", "0"); ("Width", "Height"); ("0", "Height") ];
    box "Decision" [ ("Width*0.5", "0"); ("Width", "Height*0.5"); ("Width*0.5", "Height"); ("0", "Height*0.5") ];
    (* the slant stays the same however wide the shape *)
    box "Data" ~user:[ ("User.Slant", "MIN(0.25, Width*0.2)") ]
      [ ("User.Slant", "0"); ("Width", "0"); ("Width-User.Slant", "Height"); ("0", "Height") ];
    box "Preparation" ~user:[ ("User.Cut", "MIN(Height*0.5, Width*0.25)") ]
      [ ("User.Cut", "0"); ("Width-User.Cut", "0"); ("Width", "Height*0.5"); ("Width-User.Cut", "Height"); ("User.Cut", "Height"); ("0", "Height*0.5") ];
    (* the ShapeSheet's classic: the head keeps its length *)
    box "Block arrow" ~user:[ ("User.Head", "MIN(0.5, Width*0.5)"); ("User.Shaft", "Height*0.25") ]
      [
        ("0", "Height*0.5-User.Shaft"); ("Width-User.Head", "Height*0.5-User.Shaft"); ("Width-User.Head", "0"); ("Width", "Height*0.5");
        ("Width-User.Head", "Height"); ("Width-User.Head", "Height*0.5+User.Shaft"); ("0", "Height*0.5+User.Shaft");
      ];
    { name = "Dynamic connector"; kind = Connector; cells = []; geometry = []; points = [] };
  ]

(*****************************************************************************)
(* Names into cells *)
(*****************************************************************************)

let row_of (s : shape) name =
  let rec go i = function [] -> None | (n, _) :: rest -> if n = name then Some i else go (i + 1) rest in
  go 0 s.rows

let is_start c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let is_name c = is_start c || (c >= '0' && c <= '9') || c = '.' || c = '_'

(* a formula with names into the engine's, with cells: Width into C3,
   Sheet.2!PinX into B1; a function's name kept *)
let translate (t : t) (self : shape) (text : string) : (string, string) result =
  match float_of_string_opt (String.trim text) with
  | Some _ -> Ok (String.trim text)
  | None -> (
      let n = String.length text in
      let buf = Buffer.create (n + 8) in
      let word i = let j = ref i in while !j < n && is_name text.[!j] do incr j done; !j in
      let cell (s : shape) name =
        match row_of s name with Some r -> Ok (Formula.name_of_cell (s.id, r)) | None -> Error ("no cell " ^ name)
      in
      let rec go i =
        if i >= n then Ok ()
        else if is_start text.[i] then
          let j = word i in
          let name = String.sub text i (j - i) in
          if j < n && text.[j] = '!' then
            let k = word (j + 1) in
            let other = String.sub text (j + 1) (k - j - 1) in
            let target = match int_of_string_opt (String.sub name 6 (max 0 (String.length name - 6))) with Some id when String.length name > 6 && String.sub name 0 6 = "Sheet." -> shape t id | _ -> None in
            match target with
            | None -> Error ("no shape " ^ name)
            | Some s -> ( match cell s other with Ok c -> Buffer.add_string buf c; go k | Error e -> Error e)
          else if j < n && text.[j] = '(' then begin Buffer.add_string buf (String.uppercase_ascii name); go j end
          else match cell self name with Ok c -> Buffer.add_string buf c; go j | Error e -> Error e
        else begin Buffer.add_char buf text.[i]; go (i + 1) end
      in
      match go 0 with
      | Ok () ->
          let engine = Buffer.contents buf in
          (match Formula.parse engine with Ok _ -> Ok ("=" ^ engine) | Error e -> Error e)
      | Error e -> Error e)

let replace_shape t (s : shape) = { t with shapes = List.map (fun x -> if x.id = s.id then s else x) t.shapes }

let set_formula t id name text =
  match shape t id with
  | None -> Error "no such shape"
  | Some s -> (
      match row_of s name with
      | None -> Error ("no cell " ^ name)
      | Some r -> (
          match translate t s text with
          | Error e -> Error e
          | Ok engine ->
              let s = { s with rows = List.map (fun (n, f) -> if n = name then (n, String.trim text) else (n, f)) s.rows } in
              Ok { (replace_shape t s) with sheet = Sheet.set (id, r) engine t.sheet }))

let value t id name =
  match Option.bind (shape t id) (fun s -> row_of s name) with
  | Some r -> ( match Sheet.value t.sheet (id, r) with Sheet.Number f -> Some f | _ -> None)
  | None -> None

let shown t id name = match Option.bind (shape t id) (fun s -> row_of s name) with Some r -> Sheet.show (Sheet.value t.sheet (id, r)) | None -> ""
let num t id name = Option.value (value t id name) ~default:0.
let number f = Printf.sprintf "%g" (Float.round (f *. 10000.) /. 10000.)

(* a cell set, or left as it was when the formula is refused *)
let set t id name text = match set_formula t id name text with Ok t -> t | Error _ -> t

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

let drop t (m : master) (x, y) =
  let id = t.next in
  let cells =
    match m.kind with
    | Box -> [ ("PinX", number x); ("PinY", number y) ] @ m.cells
    | Connector -> [ ("BeginX", number (x -. 0.75)); ("BeginY", number y); ("EndX", number (x +. 0.75)); ("EndY", number y) ]
  in
  (* every row named first, so that a formula can name a row below it *)
  let s = { id; master = m.name; kind = m.kind; rows = List.map (fun (n, _) -> (n, "")) cells; geometry = m.geometry; points = m.points; text = ""; glue = [] } in
  let t = { t with shapes = t.shapes @ [ s ]; next = id + 1 } in
  (List.fold_left (fun t (n, f) -> set t id n f) t cells, id)

let set_text t id text = match shape t id with Some s -> replace_shape t { s with text } | None -> t
let move t id (x, y) = set (set t id "PinX" (number x)) id "PinY" (number y)

let resize t id (w, h) =
  let left = num t id "PinX" -. (num t id "Width" /. 2.) and top = num t id "PinY" +. (num t id "Height" /. 2.) in
  let w = Float.max 0.1 w and h = Float.max 0.1 h in
  let t = set (set t id "Width" (number w)) id "Height" (number h) in
  move t id (left +. (w /. 2.), top -. (h /. 2.))

let end_cells = function Begin -> ("BeginX", "BeginY") | End -> ("EndX", "EndY")

let unglue t id e = match shape t id with Some s -> replace_shape t { s with glue = List.filter (fun (e', _, _) -> e' <> e) s.glue } | None -> t

let glue t id e ~target ~point =
  match (shape t id, shape t target) with
  | Some _, Some s when point < List.length s.points ->
      let xn, yn, _ = List.nth s.points point in
      let x, y = end_cells e in
      let sheet = Printf.sprintf "Sheet.%d!" target in
      let t = set t id x (Printf.sprintf "%sPinX-%sWidth*0.5+%s%s" sheet sheet sheet xn) in
      let t = set t id y (Printf.sprintf "%sPinY-%sHeight*0.5+%s%s" sheet sheet sheet yn) in
      let t = unglue t id e in
      (match shape t id with Some c -> replace_shape t { c with glue = (e, target, point) :: c.glue } | None -> t)
  | _ -> t

let place_end t id e (px, py) =
  let x, y = end_cells e in
  unglue (set (set t id x (number px)) id y (number py)) id e

let delete t id =
  let mention = Printf.sprintf "Sheet.%d!" id in
  let contains s sub = let n = String.length sub in let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in go 0 in
  (* what names the shape keeps its value and loses its formula *)
  let t =
    List.fold_left
      (fun t (s : shape) ->
        if s.id = id then t
        else
          let t = List.fold_left (fun t (n, f) -> if contains f mention then set t s.id n (number (num t s.id n)) else t) t s.rows in
          match shape t s.id with Some s -> replace_shape t { s with glue = List.filter (fun (_, target, _) -> target <> id) s.glue } | None -> t)
      t t.shapes
  in
  match shape t id with
  | None -> t
  | Some s ->
      let sheet = List.fold_left (fun sheet r -> Sheet.set (id, r) "" sheet) t.sheet (List.init (List.length s.rows) Fun.id) in
      { t with sheet; shapes = List.filter (fun x -> x.id <> id) t.shapes }

(*****************************************************************************)
(* On the page *)
(*****************************************************************************)

let corner t (s : shape) = (num t s.id "PinX" -. (num t s.id "Width" /. 2.), num t s.id "PinY" -. (num t s.id "Height" /. 2.))
let outline t (s : shape) = let x0, y0 = corner t s in List.map (fun (xn, yn) -> (x0 +. num t s.id xn, y0 +. num t s.id yn)) s.geometry

let connection_points t (s : shape) =
  let x0, y0 = corner t s in
  List.map (fun (xn, yn, d) -> ((x0 +. num t s.id xn, y0 +. num t s.id yn), d)) s.points

let bounds t (s : shape) =
  let x0, y0 = corner t s in
  (x0, y0, x0 +. num t s.id "Width", y0 +. num t s.id "Height")

let ends_of t (s : shape) = ((num t s.id "BeginX", num t s.id "BeginY"), (num t s.id "EndX", num t s.id "EndY"))

let route t (s : shape) =
  let b, e = ends_of t s in
  let side which =
    match List.find_opt (fun (e', _, _) -> e' = which) s.glue with
    | Some (_, target, point) -> Option.bind (shape t target) (fun x -> Option.map (fun (_, _, d) -> d) (List.nth_opt x.points point))
    | None -> None
  in
  let boxes = List.filter_map (fun (x : shape) -> if x.kind = Box then Some (bounds t x) else None) t.shapes in
  Ortho_route.route ~boxes ~margin:0.15 (b, side Begin) (e, side End)
