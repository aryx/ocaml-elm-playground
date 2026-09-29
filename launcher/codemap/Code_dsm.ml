(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_dsm.mli *)

type node = Dir of string | File of string | Def of string * int * string
type edge = { sdef : int option; dst : string; ddef : int; ename : string }

type data = {
  files : string list;
  links : (string * string * int) list;
  defs : string -> (int * string) list;
  edges : string -> edge list;
}

(* an expanded node has its parts, in order *)
type tree = Node of node * tree list option

type t = { data : data; top : tree list; mutable cache : (row list * int array array) option }
and row = { node : node; depth : int; expandable : bool }

let path_of = function Dir p | File p | Def (p, _, _) -> p
let basename p = match String.rindex_opt p '/' with Some i -> String.sub p (i + 1) (String.length p - i - 1) | None -> p
let name_of = function Dir p -> basename p ^ "/" | File p -> basename p | Def (_, _, n) -> n
let starts s p = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let under d p = d = "" || p = d || starts p (d ^ "/")

(*****************************************************************************)
(* Weights *)
(*****************************************************************************)

let files_of (d : data) (n : node) : string list =
  match n with Dir p -> List.filter (under p) d.files | File p | Def (p, _, _) -> [ p ]

let inside (n : node) (p : string) : bool = match n with Dir d -> under d p | File q | Def (q, _, _) -> p = q

(* how many uses [a] makes of [b] *)
let weight (d : data) (a : node) (b : node) : int =
  if a = b then 0
  else
    match (a, b) with
    | (Dir _ | File _), (Dir _ | File _) -> List.fold_left (fun acc (s, t, n) -> if inside a s && inside b t && s <> t then acc + n else acc) 0 d.links
    | _ ->
        List.fold_left
          (fun acc s ->
            List.fold_left
              (fun acc (e : edge) ->
                let from_ok = match a with Def (_, l, _) -> e.sdef = Some l | _ -> true in
                let to_ok = match b with Def (p, l, _) -> e.dst = p && e.ddef = l | _ -> inside b e.dst && e.dst <> s in
                if from_ok && to_ok then acc + 1 else acc)
              acc (d.edges s))
          0 (files_of d a)

(*****************************************************************************)
(* Layering: codegraph's partition *)
(*****************************************************************************)

(* what uses nothing among the rest to the front, what nothing uses to
 * the back, again and again; a cycle's in between, the fewest uses first *)
let partition (w : node -> node -> int) (nodes : node list) : node list =
  let arr = Array.of_list nodes in
  let n = Array.length arr in
  let m = Array.init n (fun i -> Array.init n (fun j -> if i = j then 0 else w arr.(i) arr.(j))) in
  let alive = Array.make n true in
  let row_empty i = let ok = ref true in for j = 0 to n - 1 do if alive.(j) && m.(i).(j) > 0 then ok := false done; !ok in
  let col_empty j = let ok = ref true in for i = 0 to n - 1 do if alive.(i) && m.(i).(j) > 0 then ok := false done; !ok in
  let left = ref [] and right = ref [] in
  let remaining () = List.filter (fun i -> alive.(i)) (List.init n Fun.id) in
  let rec loop () =
    let rows = List.filter row_empty (remaining ()) in
    if rows <> [] then begin
      List.iter (fun i -> alive.(i) <- false) rows;
      left := !left @ rows;
      loop ()
    end
    else
      let cols = List.filter col_empty (remaining ()) in
      if cols <> [] then begin
        List.iter (fun i -> alive.(i) <- false) cols;
        right := cols @ !right;
        loop ()
      end
  in
  loop ();
  let count i = Array.fold_left ( + ) 0 m.(i) in
  let cycle = List.sort (fun i j -> compare (count i) (count j)) (remaining ()) in
  List.map (fun i -> arr.(i)) (!left @ cycle @ !right)

(*****************************************************************************)
(* Expanding *)
(*****************************************************************************)

(* a node's parts: a folder's subfolders and files; a file's definitions
 * a use touches, forty at most, the most used first *)
let parts (d : data) (n : node) : node list =
  match n with
  | Dir p ->
      let prefix = if p = "" then "" else p ^ "/" in
      List.filter_map
        (fun f ->
          if not (starts f prefix) then None
          else
            let rest = String.sub f (String.length prefix) (String.length f - String.length prefix) in
            match String.index_opt rest '/' with Some i -> Some (Dir (prefix ^ String.sub rest 0 i)) | None -> Some (File f))
        d.files
      |> List.sort_uniq compare
  | File p ->
      let tally = Hashtbl.create 32 in
      let add l = Hashtbl.replace tally l (1 + Option.value (Hashtbl.find_opt tally l) ~default:0) in
      List.iter (fun (e : edge) -> Option.iter add e.sdef) (d.edges p);
      List.iter (fun (s, t, _) -> if t = p && s <> p then List.iter (fun (e : edge) -> if e.dst = p then add e.ddef) (d.edges s)) d.links;
      let defs = d.defs p in
      Hashtbl.fold (fun l k acc -> match List.find_opt (fun (l', _) -> l' = l) defs with Some (_, name) -> (k, l, name) :: acc | None -> acc) tally []
      |> List.sort (fun (a, _, _) (b, _, _) -> compare b a)
      |> List.filteri (fun i _ -> i < 40)
      |> List.map (fun (_, l, name) -> Def (p, l, name))
  | Def _ -> []

let expandable (d : data) (n : node) = match n with Def _ -> false | File p -> d.edges p <> [] || List.exists (fun (_, t, _) -> t = p) d.links | Dir _ -> true

let make (d : data) (units : string list) : t =
  let nodes = List.map (fun u -> if List.mem u d.files then File u else Dir u) units in
  { data = d; top = List.map (fun n -> Node (n, None)) (partition (weight d) nodes); cache = None }

let toggle (t : t) (target : node) : t =
  let rec go = function
    | Node (n, None) when n = target -> Node (n, Some (List.map (fun p -> Node (p, None)) (partition (weight t.data) (parts t.data n))))
    | Node (n, Some _) when n = target -> Node (n, None)
    | Node (n, Some kids) -> Node (n, Some (List.map go kids))
    | leaf -> leaf
  in
  { t with top = List.map go t.top; cache = None }

(*****************************************************************************)
(* Rows and cells *)
(*****************************************************************************)

(* the rows: the leaves, an expanded node standing for its parts *)
let leaves (t : t) : row list =
  let rec go depth = function
    | Node (_, Some (_ :: _ as kids)) -> List.concat_map (go (depth + 1)) kids
    | Node (n, _) -> [ { node = n; depth; expandable = expandable t.data n } ]
  in
  List.concat_map (go 0) t.top

let groups (t : t) : (node * int * int * int) list =
  let acc = ref [] and i = ref 0 in
  let rec go depth = function
    | Node (n, Some (_ :: _ as kids)) ->
        let first = !i in
        List.iter (go (depth + 1)) kids;
        acc := (n, first, !i - 1, depth) :: !acc
    | Node _ -> incr i
  in
  List.iter (go 0) t.top;
  List.rev !acc

let compute (t : t) =
  match t.cache with
  | Some c -> c
  | None ->
      let rows = leaves t in
      let arr = Array.of_list rows in
      let m = Array.map (fun (r : row) -> Array.map (fun (c : row) -> weight t.data r.node c.node) arr) arr in
      t.cache <- Some (rows, m);
      (rows, m)

let rows t = fst (compute t)
let matrix t = snd (compute t)

let explain (t : t) (a : node) (b : node) : (string * string * int) list =
  let tally = Hashtbl.create 16 in
  List.iter
    (fun s ->
      let defs = t.data.defs s in
      let def_name l = match List.find_opt (fun (l', _) -> l' = l) defs with Some (_, n) -> n | None -> "" in
      List.iter
        (fun (e : edge) ->
          let from_ok = match a with Def (_, l, _) -> e.sdef = Some l | _ -> true in
          let to_ok = match b with Def (p, l, _) -> e.dst = p && e.ddef = l | _ -> inside b e.dst && e.dst <> s in
          if from_ok && to_ok then
            let from = basename s ^ (match e.sdef with Some l -> "." ^ def_name l | None -> "") in
            let into = basename e.dst ^ "." ^ e.ename in
            Hashtbl.replace tally (from, into) (1 + Option.value (Hashtbl.find_opt tally (from, into)) ~default:0))
        (t.data.edges s))
    (files_of t.data a);
  Hashtbl.fold (fun (f, i) n acc -> (f, i, n) :: acc) tally [] |> List.sort (fun (_, _, x) (_, _, y) -> compare y x)

(* claude: a cell zoomed into: its row and its column alone, each expanded
 * into its parts (the author could not tell what a click on a cell did
 * when it expanded both in place, among the other rows) *)
let focus (t : t) (a : node) (b : node) : t =
  (* only the parts the cell's uses touch: the row's that use the column,
   * the column's that the row uses *)
  let keep n ps = if n = a then List.filter (fun p -> weight t.data p b > 0) ps else List.filter (fun p -> weight t.data a p > 0) ps in
  let group n = match keep n (parts t.data n) with [] -> Node (n, None) | ps -> Node (n, Some (List.map (fun p -> Node (p, None)) (partition (weight t.data) ps))) in
  let ordered = partition (weight t.data) [ a; b ] in
  { t with top = List.map group ordered; cache = None }
