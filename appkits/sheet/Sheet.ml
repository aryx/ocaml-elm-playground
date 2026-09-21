(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sheet.mli *)

module Cells = Map.Make (struct
  type t = Formula.cell

  let compare = compare
end)

type value = Number of float | Text of string | Error of string | Empty

type t = {
  (* what was typed, kept as typed: a formula that does not parse has
     to stay on the screen to be corrected *)
  raws : string Cells.t;
  contents : Formula.content Cells.t;
  values : value Cells.t;
  (* the graph, both ways round: what a cell reads, and who reads it.
     The second is the one that matters -- a change walks *forwards*
     to everything downstream of it *)
  reads : Formula.cell list Cells.t;
  read_by : Formula.cell list Cells.t;
  last_recalculated : int;
}

let empty =
  {
    raws = Cells.empty;
    contents = Cells.empty;
    values = Cells.empty;
    reads = Cells.empty;
    read_by = Cells.empty;
    last_recalculated = 0;
  }

let raw t c = match Cells.find_opt c t.raws with Some s -> s | None -> ""
let value t c = match Cells.find_opt c t.values with Some v -> v | None -> Empty
let cells t = Cells.bindings t.raws |> List.map fst
let recalculated t = t.last_recalculated

let show = function
  | Empty -> ""
  | Text s -> s
  | Error why -> "#" ^ why
  | Number f ->
      if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f
      else Printf.sprintf "%g" f

(* --- computing one cell, given the values of the cells it reads ----- *)

(* [Error] here is this module's own (a cell in error), and [Result]'s
 * is the answer of a computation that could not be done -- hence the
 * qualified constructors below, which is the price of a good name *)
let number_of : value -> (float, string) result = function
  | Number f -> Result.Ok f
  | Empty -> Result.Ok 0. (* an empty cell counts as nothing, as everywhere *)
  | Text _ -> Result.Error "text"
  | Error why -> Result.Error why

(* the numbers an argument stands for: one for a cell, many for a
 * range -- and a range's text and empty cells are skipped, which is
 * what makes SUM over a column with a heading do what you meant *)
let rec numbers t (e : Formula.expr) : (float list, string) result =
  match e with
  | Formula.Range ((c1, r1), (c2, r2)) ->
      let out = ref [] and bad = ref None in
      for col = min c1 c2 to max c1 c2 do
        for row = min r1 r2 to max r1 r2 do
          match value t (col, row) with
          | Number f -> out := f :: !out
          | Empty | Text _ -> ()
          | Error why -> if !bad = None then bad := Some why
        done
      done;
      (match !bad with Some why -> Result.Error why | None -> Result.Ok (List.rev !out))
  | e -> (
      match number_of (eval t e) with
      | Result.Ok f -> Result.Ok [ f ]
      | Result.Error why -> Result.Error why)

and eval t (e : Formula.expr) : value =
  let arith f a b =
    match (number_of (eval t a), number_of (eval t b)) with
    | Result.Ok x, Result.Ok y -> f x y
    | Result.Error why, _ | _, Result.Error why -> Error why
  in
  match e with
  | Formula.Number f -> Number f
  | Formula.Ref c -> value t c
  | Formula.Range _ -> Error "range"
  | Formula.Unary ('-', e) -> (
      match number_of (eval t e) with
      | Result.Ok f -> Number (-.f)
      | Result.Error why -> Error why)
  | Formula.Unary (_, e) -> eval t e
  | Formula.Binop ('+', a, b) -> arith (fun x y -> Number (x +. y)) a b
  | Formula.Binop ('-', a, b) -> arith (fun x y -> Number (x -. y)) a b
  | Formula.Binop ('*', a, b) -> arith (fun x y -> Number (x *. y)) a b
  | Formula.Binop ('/', a, b) ->
      arith (fun x y -> if y = 0. then Error "div0" else Number (x /. y)) a b
  | Formula.Binop (c, _, _) -> Error (Printf.sprintf "op%c" c)
  | Formula.Call (name, args) -> (
      let gathered : (float list, string) result =
        List.fold_left
          (fun (acc : (float list, string) result) arg ->
            match (acc, numbers t arg) with
            | Result.Error why, _ -> Result.Error why
            | _, Result.Error why -> Result.Error why
            | Result.Ok all, Result.Ok some -> Result.Ok (all @ some))
          (Result.Ok []) args
      in
      match gathered with
      | Result.Error why -> Error why
      | Result.Ok ns -> (
          let sum = List.fold_left ( +. ) 0. ns in
          match (name, ns) with
          | "SUM", _ -> Number sum
          | "COUNT", _ -> Number (float_of_int (List.length ns))
          | "PRODUCT", _ -> Number (List.fold_left ( *. ) 1. ns)
          | "AVERAGE", [] -> Error "empty"
          | "AVERAGE", _ -> Number (sum /. float_of_int (List.length ns))
          | "MIN", [] | "MAX", [] -> Error "empty"
          | "MIN", n :: rest -> Number (List.fold_left min n rest)
          | "MAX", n :: rest -> Number (List.fold_left max n rest)
          | _ -> Error ("fn" ^ name)))

let value_of_content t = function
  | Formula.Blank -> Empty
  | Formula.Value f -> Number f
  | Formula.Text s -> Text s
  | Formula.Formula e -> eval t e

(* --- what a change reaches, and in what order ----------------------- *)

let readers t c = match Cells.find_opt c t.read_by with Some l -> l | None -> []

(* every cell downstream of [start], itself included: the change walks
 * forwards through the graph *)
let downstream t start =
  let seen = ref [] in
  let rec go c =
    if not (List.mem c !seen) then begin
      seen := c :: !seen;
      List.iter go (readers t c)
    end
  in
  go start;
  !seen

(* Kahn's algorithm (1962), over that part of the graph only: take a
 * cell that waits for nothing, compute it, and cross it off the
 * lists of those waiting for it. What is left when nothing can be
 * taken is a cycle. *)
let recompute t dirty =
  let waiting_for c =
    match Cells.find_opt c t.reads with
    | None -> []
    | Some l -> List.filter (fun d -> List.mem d dirty) l
  in
  let pending = ref (List.map (fun c -> (c, waiting_for c)) dirty) in
  let t = ref t in
  let done_ = ref 0 in
  let rec rounds () =
    let ready, blocked = List.partition (fun (_, waits) -> waits = []) !pending in
    if ready <> [] then begin
      List.iter
        (fun (c, _) ->
          let v =
            match Cells.find_opt c !t.contents with
            | Some content -> value_of_content !t content
            | None -> Empty
          in
          t := { !t with values = Cells.add c v !t.values };
          incr done_)
        ready;
      let computed = List.map fst ready in
      pending := List.map (fun (c, waits) -> (c, List.filter (fun d -> not (List.mem d computed)) waits)) blocked;
      rounds ()
    end
  in
  rounds ();
  (* whatever is still waiting is waiting on itself, round some loop *)
  let t =
    List.fold_left
      (fun t (c, _) -> { t with values = Cells.add c (Error "cycle") t.values })
      !t !pending
  in
  { t with last_recalculated = !done_ + List.length !pending }

let set c text t =
  let content, parse_error =
    match Formula.content_of text with
    | Ok content -> (content, None)
    | Error why -> (Formula.Blank, Some why)
  in
  let old_reads = match Cells.find_opt c t.reads with Some l -> l | None -> [] in
  let new_reads = match content with Formula.Formula e -> Formula.refs e | _ -> [] in
  (* the reverse edges, taken out where they were and put in where
     they are now: the graph is only ever as right as this step *)
  let read_by =
    List.fold_left
      (fun m d ->
        let others = List.filter (fun x -> x <> c) (match Cells.find_opt d m with Some l -> l | None -> []) in
        Cells.add d others m)
      t.read_by old_reads
  in
  let read_by =
    List.fold_left
      (fun m d ->
        let others = match Cells.find_opt d m with Some l -> l | None -> [] in
        Cells.add d (c :: others) m)
      read_by new_reads
  in
  let t =
    {
      t with
      raws = (if String.trim text = "" then Cells.remove c t.raws else Cells.add c text t.raws);
      contents = Cells.add c content t.contents;
      reads = Cells.add c new_reads t.reads;
      read_by;
    }
  in
  let t = recompute t (downstream t c) in
  match parse_error with
  | None -> t
  | Some why ->
      (* the text stays on the screen, with what is wrong with it *)
      { t with raws = Cells.add c text t.raws; values = Cells.add c (Error why) t.values }

let to_string t =
  cells t
  |> List.map (fun c -> Printf.sprintf "%s\t%s" (Formula.name_of_cell c) (raw t c))
  |> String.concat "\n"

let of_string s =
  String.split_on_char '\n' s
  |> List.fold_left
       (fun t line ->
         match String.index_opt line '\t' with
         | None -> t
         | Some i -> (
             let name = String.sub line 0 i in
             let text = String.sub line (i + 1) (String.length line - i - 1) in
             match Formula.cell_of_name (String.trim name) with
             | Some c -> set c text t
             | None -> t))
       empty
