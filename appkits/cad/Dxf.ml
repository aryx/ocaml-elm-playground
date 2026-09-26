(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Dxf.mli *)

module D = Cad_drawing

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let num f =
  let s = Printf.sprintf "%.6f" f in
  if s = "-0.000000" then "0.000000" else s

let write_entity b (e : D.ent) =
  let pair code v = Buffer.add_string b (Printf.sprintf "%3d\n%s\n" code v) in
  let point base (x, y) =
    pair base (num x);
    pair (base + 10) (num y);
    pair (base + 20) (num 0.)
  in
  let start kind =
    pair 0 kind;
    pair 8 e.layer
  in
  match e.entity with
  | D.Line (a, z) ->
      start "LINE";
      point 10 a;
      point 11 z
  | D.Circle (c, r) ->
      start "CIRCLE";
      point 10 c;
      pair 40 (num r)
  | D.Arc (c, r, a0, a1) ->
      start "ARC";
      point 10 c;
      pair 40 (num r);
      pair 50 (num a0);
      pair 51 (num a1)
  | D.Insert (name, at, s, rot) ->
      start "INSERT";
      pair 2 name;
      point 10 at;
      pair 41 (num s);
      pair 42 (num s);
      pair 50 (num rot)
  | D.Dimension (p1, p2, l) ->
      start "DIMENSION";
      point 10 l;
      point 13 p1;
      point 14 p2;
      pair 70 "0";
      (* rotated: 0 degrees horizontal, 90 vertical *)
      let (x1, y1), (x2, y2), (lx, ly) = (p1, p2, l) in
      let horizontal = Float.abs (ly -. ((y1 +. y2) /. 2.)) >= Float.abs (lx -. ((x1 +. x2) /. 2.)) in
      pair 50 (if horizontal then "0.0" else "90.0")

let to_string (t : D.t) =
  let b = Buffer.create 4096 in
  let pair code v = Buffer.add_string b (Printf.sprintf "%3d\n%s\n" code v) in
  let section name body =
    pair 0 "SECTION";
    pair 2 name;
    body ();
    pair 0 "ENDSEC"
  in
  section "HEADER" (fun () ->
      pair 9 "$ACADVER";
      pair 1 "AC1009";
      pair 9 "$CLAYER";
      pair 8 t.current);
  section "TABLES" (fun () ->
      pair 0 "TABLE";
      pair 2 "LAYER";
      pair 70 (string_of_int (List.length t.layers));
      List.iter
        (fun (l : D.layer) ->
          pair 0 "LAYER";
          pair 2 l.name;
          pair 70 "0";
          (* a layer switched off has its colour negative *)
          pair 62 (string_of_int (if l.on then l.color else -l.color));
          pair 6 "CONTINUOUS")
        t.layers;
      pair 0 "ENDTAB");
  section "BLOCKS" (fun () ->
      List.iter
        (fun (name, ((bx, by), es)) ->
          pair 0 "BLOCK";
          pair 8 "0";
          pair 2 name;
          pair 70 "0";
          pair 10 (num bx);
          pair 20 (num by);
          pair 30 (num 0.);
          List.iter (write_entity b) es;
          pair 0 "ENDBLK";
          pair 8 "0")
        t.blocks);
  section "ENTITIES" (fun () -> List.iter (fun (_, e) -> write_entity b e) t.ents);
  pair 0 "EOF";
  Buffer.contents b

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

let pairs text =
  let lines = String.split_on_char '\n' text |> List.map String.trim in
  let rec go acc = function
    | [] | [ "" ] -> Ok (List.rev acc)
    | [ _ ] -> Error "a group code without its value"
    | code :: v :: rest -> (
        match int_of_string_opt code with Some c -> go ((c, v) :: acc) rest | None -> Error ("not a group code: " ^ code))
  in
  go [] lines

(* the pairs cut at each code 0: a thing, its type and its pairs *)
let rec things = function
  | [] -> []
  | (0, kind) :: rest ->
      let rec body acc = function (0, _) :: _ as l -> (List.rev acc, l) | p :: l -> body (p :: acc) l | [] -> (List.rev acc, []) in
      let b, rest = body [] rest in
      (kind, b) :: things rest
  | _ :: rest -> things rest

let float_of ps code default = match List.assoc_opt code ps with Some v -> Option.value (float_of_string_opt v) ~default | None -> default
let point ps base = (float_of ps base 0., float_of ps (base + 10) 0.)

(* one entity's pairs, as entities (a polyline as its lines) *)
let entity kind ps =
  let layer = Option.value (List.assoc_opt 8 ps) ~default:"0" in
  let one e = [ { D.entity = e; layer } ] in
  match kind with
  | "LINE" -> one (D.Line (point ps 10, point ps 11))
  | "CIRCLE" -> one (D.Circle (point ps 10, float_of ps 40 0.))
  | "ARC" -> one (D.Arc (point ps 10, float_of ps 40 0., float_of ps 50 0., float_of ps 51 0.))
  | "INSERT" -> (
      match List.assoc_opt 2 ps with
      | Some name -> one (D.Insert (name, point ps 10, float_of ps 41 1., float_of ps 50 0.))
      | None -> [])
  | "DIMENSION" -> one (D.Dimension (point ps 13, point ps 14, point ps 10))
  | "LWPOLYLINE" ->
      (* its vertices: each x (code 10) followed by its y (code 20) *)
      let rec vertices = function
        | (10, x) :: (20, y) :: rest -> (
            match (float_of_string_opt x, float_of_string_opt y) with
            | Some x, Some y -> (x, y) :: vertices rest
            | _ -> vertices rest)
        | _ :: rest -> vertices rest
        | [] -> []
      in
      let pts = vertices ps in
      let closed = int_of_float (float_of ps 70 0.) land 1 = 1 in
      let rec segs = function a :: (b :: _ as rest) -> { D.entity = D.Line (a, b); layer } :: segs rest | _ -> [] in
      segs (if closed && pts <> [] then pts @ [ List.hd pts ] else pts)
  | _ -> []

let of_string text =
  match pairs text with
  | Error e -> Error e
  | Ok ps ->
      let ts = things ps in
      (* the section each thing is in *)
      let rec walk section acc (t : D.t) block = function
        | [] -> Ok t
        | ("SECTION", b) :: rest -> walk (Option.value (List.assoc_opt 2 b) ~default:"") acc t block rest
        | ("ENDSEC", _) :: rest -> walk "" acc t block rest
        | ("EOF", _) :: _ -> Ok t
        | ("LAYER", b) :: rest when section = "TABLES" ->
            let color = int_of_float (float_of b 62 7.) in
            let l = { D.name = Option.value (List.assoc_opt 2 b) ~default:"0"; color = abs color; on = color >= 0 } in
            walk section acc (D.set_layer l t) block rest
        | ("BLOCK", b) :: rest when section = "BLOCKS" ->
            walk section [] t (Some (Option.value (List.assoc_opt 2 b) ~default:"", point b 10)) rest
        | ("ENDBLK", _) :: rest when section = "BLOCKS" -> (
            match block with
            | Some (name, base) -> walk section [] { t with blocks = t.blocks @ [ (name, (base, List.rev acc)) ] } None rest
            | None -> walk section [] t None rest)
        | (kind, b) :: rest when section = "BLOCKS" -> walk section (List.rev_append (entity kind b) acc) t block rest
        | (kind, b) :: rest when section = "ENTITIES" ->
            let t =
              List.fold_left
                (fun t (e : D.ent) ->
                  let t = D.ensure_layer e.layer t in
                  let current = t.current in
                  let t, _ = D.add e.entity { t with current = e.layer } in
                  { t with current })
                t (entity kind b)
            in
            walk section acc t block rest
        | _ :: rest -> walk section acc t block rest
      in
      (* the header's $CLAYER: the pairs after it *)
      let current =
        let rec find = function (9, "$CLAYER") :: (8, l) :: _ -> Some l | _ :: rest -> find rest | [] -> None in
        find ps
      in
      Result.map
        (fun (t : D.t) ->
          match current with Some l when List.exists (fun (x : D.layer) -> x.name = l) t.layers -> { t with current = l } | _ -> t)
        (walk "" [] D.empty None ts)
