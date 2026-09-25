(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Teletype
open Basic_parse

(* See Basic_run.mli *)

(*****************************************************************************)
(* Programs *)
(*****************************************************************************)

module IMap = Map.Make (Int)

type dialect = Integer | Applesoft
type program = (string * stmt list) IMap.t

let empty = IMap.empty
let add (p : program) n text stmts = IMap.add n (text, stmts) p
let remove (p : program) n = IMap.remove n p
let listing (p : program) : string = IMap.fold (fun n (text, _) acc -> acc ^ Printf.sprintf "%d %s\n" n text) p ""

let text_of (line : string) : string =
  let s = String.trim (capitals line) in
  let i = ref 0 in
  while !i < String.length s && s.[!i] >= '0' && s.[!i] <= '9' do
    incr i
  done;
  String.trim (String.sub s !i (String.length s - !i))

let of_lines (lines : string list) : (program, string) result =
  List.fold_left
    (fun acc line ->
      match acc, parse_line line with
      | Error _, _ -> acc
      | Ok p, Ok (Numbered (n, Some stmts)) -> Ok (add p n (text_of line) stmts)
      | Ok _, Ok _ -> Error ("NOT A NUMBERED LINE: " ^ line)
      | Ok _, Error msg -> Error (msg ^ ": " ^ line))
    (Ok empty) lines

(*****************************************************************************)
(* Values *)
(*****************************************************************************)

type value = N of float | S of string

(* 16 bits, two's complement: 32767 + 1 = -32768 *)
let wrap (x : float) : float =
  let n = int_of_float (Float.trunc x) in
  float_of_int (((n + 32768) land 0xFFFF) - 32768)

(* nine significant digits, no 0 before the point, Applesoft's way *)
let show_number (d : dialect) (x : float) : string =
  if Float.is_integer x && Float.abs x < 1e9 then Printf.sprintf "%.0f" x
  else if d = Integer then Printf.sprintf "%.0f" (wrap x)
  else
    let s = Printf.sprintf "%.9g" x |> String.map (fun c -> if c = 'e' then 'E' else c) in
    if String.length s > 1 && s.[0] = '0' && s.[1] = '.' then String.sub s 1 (String.length s - 1)
    else if String.length s > 2 && String.sub s 0 3 = "-0." then "-" ^ String.sub s 2 (String.length s - 2)
    else s

(* only the first two characters count, and the $ *)
let key (name : string) : string =
  let base = if is_string name then String.sub name 0 (String.length name - 1) else name in
  let base = if String.length base > 2 then String.sub base 0 2 else base in
  if is_string name then base ^ "$" else base

let default (name : string) : value = if is_string name then S "" else N 0.

(*****************************************************************************)
(* The machine *)
(*****************************************************************************)

type loop = { var : string; limit : float; step_by : float; back : int * int }

(* mutable, one per RUN: each continuation is taken once *)
type machine = {
  dialect : dialect;
  (* the lines, and after them the direct statements' line, if any *)
  code : (int * stmt array) array;
  lines : int; (* how many of [code] are the program's *)
  index : (int, int) Hashtbl.t;
  vars : (string, value) Hashtbl.t;
  arrays : (string, int list * value array) Hashtbl.t;
  fns : (string, string * expr) Hashtbl.t;
  mutable gosubs : (int * int) list;
  mutable loops : loop list;
  data : datum array;
  mutable next_datum : int;
  mutable col : int;
}

exception Error of string

let fail msg = raise (Error msg)

let create (dialect : dialect) (p : program) (direct : stmt list option) : machine =
  let lines = IMap.fold (fun n (_, s) acc -> (n, Array.of_list s) :: acc) p [] |> List.rev in
  let code = Array.of_list (lines @ match direct with Some s -> [ (-1, Array.of_list s) ] | None -> []) in
  let index = Hashtbl.create 64 in
  Array.iteri (fun i (n, _) -> if n >= 0 then Hashtbl.replace index n i) code;
  let data = List.concat_map (fun (_, s) -> Array.to_list s |> List.concat_map (function Data d -> d | _ -> [])) lines in
  { dialect; code; lines = List.length lines; index; vars = Hashtbl.create 32; arrays = Hashtbl.create 8; fns = Hashtbl.create 4;
    gosubs = []; loops = []; data = Array.of_list data; next_datum = 0; col = 0 }

(* a number as the dialect keeps it: truncated and wrapped in Integer *)
let num (m : machine) (x : float) : value = N (if m.dialect = Integer then wrap x else x)

let number_of (v : value) : float = match v with N x -> x | S _ -> fail "TYPE MISMATCH"
let string_of (v : value) : string = match v with S s -> s | N _ -> fail "TYPE MISMATCH"

(* an array's element, the array made with 11 per dimension when used
   without a DIM *)
let element (m : machine) (name : string) (subs : int list) : value array * int =
  let k = key name in
  let dims, cells =
    match Hashtbl.find_opt m.arrays k with
    | Some a -> a
    | None ->
        let dims = List.map (fun _ -> 10) subs in
        let a = (dims, Array.make (List.fold_left (fun n d -> n * (d + 1)) 1 dims) (default name)) in
        Hashtbl.replace m.arrays k a;
        a
  in
  if List.length dims <> List.length subs then fail "BAD SUBSCRIPT";
  let i = List.fold_left2 (fun acc d s -> if s < 0 || s > d then fail "BAD SUBSCRIPT" else (acc * (d + 1)) + s) 0 dims subs in
  (cells, i)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* The one thing an expression can't compute alone is RND, a random
   number from Teletype's generator (the seeded one, so a game is the
   same from the same seed): so an expression is a Teletype
   computation. Its errors are OCaml exceptions ([fail]), caught by
   [value] below *)
let ( let+ ) (m : 'a talk) (f : 'a -> 'b) : 'b talk =
  let* x = m in
  return (f x)

let rec eval (m : machine) (e : expr) : value talk =
  match e with
  | Num x -> return (num m x)
  | Str s -> return (S s)
  | Var v -> return (Option.value (Hashtbl.find_opt m.vars (key v)) ~default:(default v))
  | Index (a, subs) ->
      let+ subs = eval_ints m subs in
      let cells, i = element m a subs in
      cells.(i)
  | Neg e ->
      let+ v = eval m e in
      num m (-.number_of v)
  | Not e ->
      let+ v = eval m e in
      N (if number_of v = 0. then 1. else 0.)
  | Bin (op, a, b) ->
      let* x = eval m a in
      let* y = eval m b in
      return (binary m op x y)
  | Fn (f, arg) -> (
      match Hashtbl.find_opt m.fns (key f) with
      | None -> fail "UNDEF'D FUNCTION"
      | Some (x, body) ->
          let* v = eval m arg in
          (* the parameter a variable for the time of the call *)
          let saved = Hashtbl.find_opt m.vars (key x) in
          Hashtbl.replace m.vars (key x) v;
          let+ r = eval m body in
          (match saved with Some s -> Hashtbl.replace m.vars (key x) s | None -> Hashtbl.remove m.vars (key x));
          r)
  | Call ("RND", [ a ]) -> (
      let* v = eval m a in
      match m.dialect, v with
      | Integer, N n when n >= 1. ->
          let+ r = random (int_of_float n) in
          N (float_of_int (r + 1))
      | Applesoft, N _ ->
          let+ r = random 16777216 in
          N (float_of_int r /. 16777216.)
      | _ -> fail "ILLEGAL QUANTITY")
  | Call (f, args) ->
      let* vs = eval_all m args in
      return (call m f vs)

and eval_all (m : machine) (es : expr list) : value list talk =
  match es with
  | [] -> return []
  | e :: rest ->
      let* v = eval m e in
      let+ vs = eval_all m rest in
      v :: vs

and eval_ints (m : machine) (es : expr list) : int list talk =
  let+ vs = eval_all m es in
  List.map (fun v -> int_of_float (number_of v)) vs

and binary (m : machine) (op : op) (x : value) (y : value) : value =
  let truth b = N (if b then 1. else 0.) in
  match op, x, y with
  | Add, S a, S b -> S (a ^ b)
  | (Eq | Ne | Lt | Le | Gt | Ge), S a, S b ->
      let c = compare a b in
      truth (match op with Eq -> c = 0 | Ne -> c <> 0 | Lt -> c < 0 | Le -> c <= 0 | Gt -> c > 0 | _ -> c >= 0)
  | _ -> (
      let a = number_of x and b = number_of y in
      match op with
      | Add -> num m (a +. b)
      | Sub -> num m (a -. b)
      | Mul -> num m (a *. b)
      | Div -> if b = 0. then fail "DIVISION BY ZERO" else num m (a /. b)
      | Pow -> num m (Float.pow a b)
      | Eq -> truth (a = b)
      | Ne -> truth (a <> b)
      | Lt -> truth (a < b)
      | Le -> truth (a <= b)
      | Gt -> truth (a > b)
      | Ge -> truth (a >= b)
      | And -> truth (a <> 0. && b <> 0.)
      | Or -> truth (a <> 0. || b <> 0.))

and call (m : machine) (f : string) (vs : value list) : value =
  let n1 () = match vs with [ v ] -> number_of v | _ -> fail "SYNTAX" in
  let s1 () = match vs with [ v ] -> string_of v | _ -> fail "SYNTAX" in
  let positive x = if x < 0. then fail "ILLEGAL QUANTITY" else x in
  match f, vs with
  | "INT", _ -> num m (Float.floor (n1 ()))
  | "ABS", _ -> num m (Float.abs (n1 ()))
  | "SGN", _ -> N (let x = n1 () in if x > 0. then 1. else if x < 0. then -1. else 0.)
  | "SQR", _ -> num m (Float.sqrt (positive (n1 ())))
  | "SIN", _ -> num m (Float.sin (n1 ()))
  | "COS", _ -> num m (Float.cos (n1 ()))
  | "TAN", _ -> num m (Float.tan (n1 ()))
  | "ATN", _ -> num m (Float.atan (n1 ()))
  | "EXP", _ -> num m (Float.exp (n1 ()))
  | "LOG", _ -> let x = n1 () in if x <= 0. then fail "ILLEGAL QUANTITY" else num m (Float.log x)
  | "LEN", _ -> N (float_of_int (String.length (s1 ())))
  | "VAL", _ ->
      (* the longest number at the start, 0 if none *)
      let s = String.trim (s1 ()) in
      let rec longest n = if n = 0 then 0. else match float_of_string_opt (String.sub s 0 n) with Some x -> x | None -> longest (n - 1) in
      num m (longest (String.length s))
  | "ASC", _ -> let s = s1 () in if s = "" then fail "ILLEGAL QUANTITY" else N (float_of_int (Char.code s.[0]))
  | "CHR$", _ -> let x = int_of_float (n1 ()) in if x < 0 || x > 255 then fail "ILLEGAL QUANTITY" else S (String.make 1 (Char.chr x))
  | "STR$", _ -> S (show_number m.dialect (n1 ()))
  | "LEFT$", [ s; n ] -> let s = string_of s and n = int_of_float (positive (number_of n)) in S (String.sub s 0 (min n (String.length s)))
  | "RIGHT$", [ s; n ] ->
      let s = string_of s and n = int_of_float (positive (number_of n)) in
      let n = min n (String.length s) in
      S (String.sub s (String.length s - n) n)
  | "MID$", s :: start :: len ->
      let s = string_of s and start = int_of_float (number_of start) in
      if start < 1 then fail "ILLEGAL QUANTITY";
      let from = min (start - 1) (String.length s) in
      let len = match len with [ l ] -> int_of_float (positive (number_of l)) | _ -> String.length s in
      S (String.sub s from (min len (String.length s - from)))
  | _ -> fail "SYNTAX"

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

(* An expression's error is raised while [eval] builds its computation,
   caught by the [try] around it -- except after an RND, whose
   continuation the machine runs later, outside any [try] here. So
   [value] wraps each continuation in a handler of its own: a value, or
   the error's message, whenever it comes. *)
let value (m : machine) (e : expr) : (value, string) result talk =
  let rec guard (t : value talk) : (value, string) result talk =
    match t with
    | Done v -> return (Ok v)
    | Print (s, k) -> Print (s, guard k)
    | Random (n, k) -> Random (n, fun i -> try guard (k i) with Error msg -> return (Result.Error msg))
    | Step k -> Step (fun () -> try guard (k ()) with Error msg -> return (Result.Error msg))
    | Read_line k -> Read_line (fun l -> try guard (k l) with Error msg -> return (Result.Error msg))
    | Read_key k -> Read_key (fun l -> try guard (k l) with Error msg -> return (Result.Error msg))
    | Spawn (c, k) -> Spawn (c, fun st -> try guard (k st) with Error msg -> return (Result.Error msg))
  in
  try guard (eval m e) with Error msg -> return (Result.Error msg)

let line_of (m : machine) (li : int) : int option = if li < m.lines then Some (fst m.code.(li)) else None

let report (m : machine) (li : int) (msg : string) : unit talk =
  let at = match line_of m li with Some n -> Printf.sprintf " IN %d" n | None -> "" in
  let text = match m.dialect with Integer -> Printf.sprintf "*** %s ERR%s\n" msg at | Applesoft -> Printf.sprintf "?%s ERROR%s\n" msg at in
  let fresh_line = if m.col > 0 then "\n" else "" in
  m.col <- 0;
  print (fresh_line ^ text)

let assign (m : machine) (lv : lvalue) (subs : int list) (v : value) : unit =
  let name = match lv with Scalar n | Elem (n, _) -> n in
  (match is_string name, v with true, N _ | false, S _ -> fail "TYPE MISMATCH" | _ -> ());
  match lv with
  | Scalar n -> Hashtbl.replace m.vars (key n) v
  | Elem (n, _) ->
      let cells, i = element m n subs in
      cells.(i) <- v

(* an INPUT's field as the variable's value, None if it isn't one *)
let field_value (name : string) (field : string) : value option =
  let f = String.trim field in
  let f = if String.length f >= 2 && f.[0] = '"' && f.[String.length f - 1] = '"' then String.sub f 1 (String.length f - 2) else f in
  if is_string name then Some (S f) else match float_of_string_opt f with Some x when f <> "" -> Some (N x) | _ -> None

let rec go (m : machine) (li : int) (si : int) : unit talk =
  let* () = step in
  if li >= Array.length m.code then return ()
  else
    let _, stmts = m.code.(li) in
    if si >= Array.length stmts then
      (* the program's lines, one after the other; a direct line doesn't
         fall into them, nor the program into it *)
      if li + 1 < m.lines then go m (li + 1) 0 else return ()
    else try exec m li si stmts.(si) with Error msg -> report m li msg

and exec (m : machine) (li : int) (si : int) (s : stmt) : unit talk =
  let next () = go m li (si + 1) in
  let next_line () = if li + 1 < m.lines then go m (li + 1) 0 else return () in
  (* a value, or the error reported and the program stopped *)
  let with_value e (f : value -> unit talk) : unit talk =
    let* r = value m e in
    match r with Ok v -> (try f v with Error msg -> report m li msg) | Result.Error msg -> report m li msg
  in
  let with_values es (f : value list -> unit talk) : unit talk =
    let rec all es acc = match es with [] -> f (List.rev acc) | e :: rest -> with_value e (fun v -> all rest (v :: acc)) in
    all es []
  in
  let jump (target : int) (m : machine) : unit talk =
    match Hashtbl.find_opt m.index target with Some i -> go m i 0 | None -> report m li (Printf.sprintf "NO LINE %d" target)
  in
  let zone = match m.dialect with Integer -> 8 | Applesoft -> 16 in
  match s with
  | Print items ->
      let rec items_out (items : (item * sep) list) (acc : string) : unit talk =
        match items with
        | [] ->
            let* () = print acc in
            next ()
        | (item, sep) :: rest ->
            let text_of_item (f : string -> unit talk) : unit talk =
              match item with
              | Expr e -> with_value e (fun v -> f (match v with N x -> show_number m.dialect x | S s -> s))
              | Tab e -> with_value e (fun v -> f (String.make (max 0 (int_of_float (number_of v) - 1 - m.col)) ' '))
              | Spc e -> with_value e (fun v -> f (String.make (max 0 (int_of_float (number_of v))) ' '))
            in
            text_of_item (fun text ->
                m.col <- m.col + String.length text;
                let after =
                  match sep with
                  | Semi -> ""
                  | Newline ->
                      m.col <- 0;
                      "\n"
                  | Comma ->
                      let pad = zone - (m.col mod zone) in
                      m.col <- m.col + pad;
                      String.make pad ' '
                in
                items_out rest (acc ^ text ^ after))
      in
      if items = [] then begin
        m.col <- 0;
        let* () = print "\n" in
        next ()
      end
      else items_out items ""
  | Input (prompt, lvs) ->
      (* the subscripts first, then the line, split at its commas *)
      let subs_of lv = match lv with Scalar _ -> [] | Elem (_, es) -> es in
      with_values (List.concat_map subs_of lvs) (fun subs ->
          let subs = List.map (fun v -> int_of_float (number_of v)) subs in
          let rec split lvs subs =
            match lvs with
            | [] -> []
            | lv :: rest ->
                let n = List.length (subs_of lv) in
                (lv, List.filteri (fun i _ -> i < n) subs) :: split rest (List.filteri (fun i _ -> i >= n) subs)
          in
          let targets = split lvs subs in
          let rec ask_for (question : string) (targets : (lvalue * int list) list) : unit talk =
            let* line = ask question in
            let fields = String.split_on_char ',' line in
            let rec take targets fields =
              match targets, fields with
              | [], [] -> next ()
              | [], _ :: _ ->
                  let* () = print "?EXTRA IGNORED\n" in
                  next ()
              | _ :: _, [] -> ask_for "?? " targets
              | (lv, s) :: trest, f :: frest -> (
                  let name = match lv with Scalar n | Elem (n, _) -> n in
                  match field_value name f with
                  | Some v -> (
                      (* in a Read_line's continuation: its own handler *)
                      match assign m lv s v with
                      | () -> take trest frest
                      | exception Error msg -> report m li msg)
                  | None ->
                      let* () = print "?REENTER\n" in
                      ask_for "? " targets)
            in
            m.col <- 0;
            take targets fields
          in
          ask_for (match prompt with Some p -> p ^ "? " | None -> "? ") targets)
  | Let (lv, e) ->
      let subs = match lv with Scalar _ -> [] | Elem (_, es) -> es in
      with_values subs (fun subs ->
          with_value e (fun v ->
              assign m lv (List.map (fun v -> int_of_float (number_of v)) subs) v;
              next ()))
  | If c -> with_value c (fun v -> if (match v with N x -> x <> 0. | S s -> s <> "") then next () else next_line ())
  | Goto e -> with_value e (fun v -> jump (int_of_float (number_of v)) m)
  | Gosub e ->
      with_value e (fun v ->
          m.gosubs <- (li, si + 1) :: m.gosubs;
          jump (int_of_float (number_of v)) m)
  | On (e, sub, targets) ->
      with_value e (fun v ->
          let k = int_of_float (number_of v) in
          if k < 0 then fail "ILLEGAL QUANTITY"
          else if k = 0 || k > List.length targets then next ()
          else begin
            if sub then m.gosubs <- (li, si + 1) :: m.gosubs;
            jump (List.nth targets (k - 1)) m
          end)
  | Return -> (
      match m.gosubs with
      | (l, s) :: rest ->
          m.gosubs <- rest;
          go m l s
      | [] -> fail "RETURN WITHOUT GOSUB")
  | For (v, a, b, step_by) ->
      with_values (a :: b :: Option.to_list step_by) (fun vs ->
          let start, limit, by =
            match vs with [ a; b ] -> (a, number_of b, 1.) | [ a; b; c ] -> (a, number_of b, number_of c) | _ -> assert false
          in
          assign m (Scalar v) [] start;
          (* a loop on the same variable replaces the old one, and the
             loops started inside it *)
          let rec drop = function [] -> [] | l :: rest -> if l.var = key v then rest else drop rest in
          let others = if List.exists (fun l -> l.var = key v) m.loops then drop m.loops else m.loops in
          m.loops <- { var = key v; limit; step_by = by; back = (li, si + 1) } :: others;
          next ())
  | Next vs ->
      let rec each (vs : string list) : unit talk =
        (* the loop named, or the innermost; the ones inside it closed *)
        let rec find = function
          | [] -> fail "NEXT WITHOUT FOR"
          | l :: rest -> (match vs with [] -> (l, rest) | v :: _ -> if l.var = key v then (l, rest) else find rest)
        in
        let l, outer = find m.loops in
        let x = number_of (Option.value (Hashtbl.find_opt m.vars l.var) ~default:(N 0.)) +. l.step_by in
        Hashtbl.replace m.vars l.var (num m x);
        if (l.step_by >= 0. && x <= l.limit) || (l.step_by < 0. && x >= l.limit) then begin
          m.loops <- l :: outer;
          go m (fst l.back) (snd l.back)
        end
        else begin
          m.loops <- outer;
          match vs with _ :: (_ :: _ as rest) -> each rest | _ -> next ()
        end
      in
      each vs
  | Dim arrays ->
      let rec each = function
        | [] -> next ()
        | (name, dims) :: rest ->
            with_values dims (fun ds ->
                let ds = List.map (fun v -> int_of_float (number_of v)) ds in
                if Hashtbl.mem m.arrays (key name) then fail "REDIM'D ARRAY";
                if List.exists (fun d -> d < 0) ds then fail "ILLEGAL QUANTITY";
                Hashtbl.replace m.arrays (key name) (ds, Array.make (List.fold_left (fun n d -> n * (d + 1)) 1 ds) (default name));
                each rest)
      in
      each arrays
  | Data _ | Rem -> next ()
  | Read lvs ->
      let rec each = function
        | [] -> next ()
        | lv :: rest ->
            let subs = match lv with Scalar _ -> [] | Elem (_, es) -> es in
            with_values subs (fun subs ->
                if m.next_datum >= Array.length m.data then fail "OUT OF DATA";
                let d = m.data.(m.next_datum) in
                m.next_datum <- m.next_datum + 1;
                let name = match lv with Scalar n | Elem (n, _) -> n in
                let v = match d, is_string name with D_num x, false -> num m x | D_str s, true -> S s | D_num x, true -> S (show_number m.dialect x) | D_str _, false -> fail "TYPE MISMATCH" in
                assign m lv (List.map (fun v -> int_of_float (number_of v)) subs) v;
                each rest)
      in
      each lvs
  | Restore ->
      m.next_datum <- 0;
      next ()
  | Def (f, x, body) ->
      Hashtbl.replace m.fns (key f) (x, body);
      next ()
  | End -> return ()
  | Stop -> print (Printf.sprintf "%sBREAK%s\n" (if m.col > 0 then "\n" else "") (match line_of m li with Some n -> Printf.sprintf " IN %d" n | None -> ""))
  | List | Run | New | Bye | Fp | Int | Catalog | Load _ | Save _ | Run_file _ -> fail "NOT IN A PROGRAM"

let run (d : dialect) (p : program) : unit talk =
  let m = create d p None in
  if m.lines = 0 then return () else go m 0 0

let direct (d : dialect) (p : program) (stmts : stmt list) : unit talk =
  let m = create d p (Some stmts) in
  go m m.lines 0
