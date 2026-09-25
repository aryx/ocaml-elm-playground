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

type program = (string * stmt) IMap.t

let empty = IMap.empty
let add (p : program) n text stmt = IMap.add n (text, stmt) p
let remove (p : program) n = IMap.remove n p
let listing (p : program) : string = IMap.fold (fun n (text, _) acc -> acc ^ Printf.sprintf "%d %s\n" n text) p ""

let of_lines (lines : string list) : (program, string) result =
  List.fold_left
    (fun acc line ->
      match acc with
      | Error _ -> acc
      | Ok p -> (
          match parse_line line with
          | Ok (Numbered (n, Some stmt)) ->
              (* the text after the number, as typed, in capitals *)
              let text = String.trim (capitals line) in
              let i = ref 0 in
              while !i < String.length text && text.[!i] >= '0' && text.[!i] <= '9' do
                incr i
              done;
              Ok (add p n (String.trim (String.sub text !i (String.length text - !i))) stmt)
          | Ok _ -> Error ("NOT A NUMBERED LINE: " ^ line)
          | Error msg -> Error (msg ^ ": " ^ line)))
    (Ok empty) lines

(*****************************************************************************)
(* Numbers *)
(*****************************************************************************)

(* 16 bits, two's complement: 32767 + 1 = -32768 *)
let wrap (n : int) : int = ((n + 32768) land 0xFFFF) - 32768

(* a computation that may fail: its error stops the program *)
let ( let*? ) (m : ('a, string) result talk) (f : 'a -> ('b, string) result talk) : ('b, string) result talk =
  let* r = m in
  match r with Ok x -> f x | Error e -> return (Error e)

let ok x = return (Ok x)

let rec eval (vars : int array) (e : expr) : (int, string) result talk =
  match e with
  | Num n -> ok n
  | Var c -> ok vars.(Char.code c - Char.code 'A')
  | Neg e ->
      let*? x = eval vars e in
      ok (wrap (-x))
  | Abs e ->
      let*? x = eval vars e in
      ok (wrap (abs x))
  | Rnd e ->
      let*? n = eval vars e in
      if n <= 0 then return (Error "RND RANGE")
      else
        let* r = random n in
        ok (r + 1)
  | Bin (op, a, b) -> (
      let*? x = eval vars a in
      let*? y = eval vars b in
      match op with
      | Add -> ok (wrap (x + y))
      | Sub -> ok (wrap (x - y))
      | Mul -> ok (wrap (x * y))
      | Div -> if y = 0 then return (Error "DIVISION BY ZERO") else ok (wrap (x / y)))

let holds (r : relop) (x : int) (y : int) : bool =
  match r with Eq -> x = y | Ne -> x <> y | Lt -> x < y | Le -> x <= y | Gt -> x > y | Ge -> x >= y

(*****************************************************************************)
(* The machine *)
(*****************************************************************************)

(* the variables, the GOSUBs' return points (indexes in [lines]), the
   column the printing is at (for ",") *)
type state = { vars : int array; stack : int list; col : int }

let fresh () : state = { vars = Array.make 26 0; stack = []; col = 0 }

(* the printing's text, and the column after it *)
let rec print_items (vars : int array) (col : int) (items : (item * sep) list) : (string * int, string) result talk =
  match items with
  | [] -> ok ("", col)
  | (item, sep) :: rest ->
      let*? text = match item with Str s -> ok s | Expr e -> let*? n = eval vars e in ok (string_of_int n) in
      let col = col + String.length text in
      let text, col =
        match sep with
        | Semi -> (text, col)
        | Comma -> (text ^ String.make (8 - (col mod 8)) ' ', col + 8 - (col mod 8))
        | Newline -> (text ^ "\n", 0)
      in
      let*? more, col = print_items vars col rest in
      ok (text ^ more, col)

(* a number typed for INPUT, asked again until it is one *)
let rec input_number () : int talk =
  let* answer = ask "? " in
  let s = String.trim answer in
  let digits = if String.length s > 0 && (s.[0] = '-' || s.[0] = '+') then String.sub s 1 (String.length s - 1) else s in
  match int_of_string_opt s with
  | Some n when digits <> "" && String.for_all (fun c -> c >= '0' && c <= '9') digits -> return (wrap n)
  | _ ->
      let* () = print "?REENTER\n" in
      input_number ()

(* the program's lines as an array, and where each line number is *)
type code = { lines : (int * stmt) array; index : (int, int) Hashtbl.t }

let compile (p : program) : code =
  let lines = Array.of_list (IMap.fold (fun n (_, s) acc -> (n, s) :: acc) p [] |> List.rev) in
  let index = Hashtbl.create 64 in
  Array.iteri (fun i (n, _) -> Hashtbl.replace index n i) lines;
  { lines; index }

(* the statement at [pc] and on: a step each, so that a long run
   leaves the machine its frames *)
let rec go (code : code) (pc : int) (st : state) : unit talk =
  let* () = step in
  if pc >= Array.length code.lines then return ()
  else
    let n, s = code.lines.(pc) in
    exec code (Some n) s (fun st -> go code (pc + 1) st) st

(* one statement; [next] is what follows it, [where] its line (None
   for a direct command) *)
and exec (code : code) (where : int option) (s : stmt) (next : state -> unit talk) (st : state) : unit talk =
  let error msg =
    let at = match where with Some n -> Printf.sprintf " ERR IN %d" n | None -> " ERR" in
    print (Printf.sprintf "%s*** %s%s\n" (if st.col > 0 then "\n" else "") msg at)
  in
  let jump (e : expr) (st : state) : unit talk =
    let* r = eval st.vars e in
    match r with
    | Error msg -> error msg
    | Ok target -> (
        match Hashtbl.find_opt code.index target with
        | Some pc -> go code pc st
        | None -> error (Printf.sprintf "NO LINE %d" target))
  in
  match s with
  (* PRINT alone: an empty line *)
  | Print [] ->
      let* () = print "\n" in
      next { st with col = 0 }
  | Print items -> (
      let* r = print_items st.vars st.col items in
      match r with
      | Error msg -> error msg
      | Ok (text, col) ->
          let* () = print text in
          next { st with col })
  | Input vs ->
      let rec each vs (st : state) =
        match vs with
        | [] -> next { st with col = 0 }
        | v :: rest ->
            let* n = input_number () in
            let vars = Array.copy st.vars in
            vars.(Char.code v - Char.code 'A') <- n;
            each rest { st with vars; col = 0 }
      in
      each vs st
  | Let (v, e) -> (
      let* r = eval st.vars e in
      match r with
      | Error msg -> error msg
      | Ok n ->
          let vars = Array.copy st.vars in
          vars.(Char.code v - Char.code 'A') <- n;
          next { st with vars })
  | If (a, r, b, then_) -> (
      let* x = eval st.vars a in
      let* y = eval st.vars b in
      match x, y with
      | Ok x, Ok y -> if holds r x y then exec code where then_ next st else next st
      | Error msg, _ | _, Error msg -> error msg)
  | Goto e -> jump e st
  | Gosub e ->
      (* where to come back: the line after this one *)
      let back = match where with Some n -> Hashtbl.find code.index n + 1 | None -> Array.length code.lines in
      jump e { st with stack = back :: st.stack }
  | Return -> (
      match st.stack with
      | pc :: stack -> go code pc { st with stack }
      | [] -> error "RETURN WITHOUT GOSUB")
  | End -> return ()
  | Rem -> next st
  | List | Run | New | Bye -> error "NOT IN A PROGRAM"

let run (p : program) : unit talk = go (compile p) 0 (fresh ())
let direct (p : program) (s : stmt) : unit talk = exec (compile p) None s (fun _ -> return ()) (fresh ())
