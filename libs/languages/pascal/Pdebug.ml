(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pdebug.mli *)

(*****************************************************************************)
(* Where we are *)
(*****************************************************************************)

let procedure_at (p : Pcode.program) (pc : int) : int =
  let found = ref 0 in
  Array.iteri (fun i (q : Pcode.procedure) -> if pc >= q.first && pc <= q.last then found := i) p.procedures;
  !found

let line (p : Pcode.program) (m : Pmachine.machine) : int =
  let pc = Pmachine.pc m in
  if pc >= Array.length p.code then 0 else if p.statements.(pc) >= 0 then p.statements.(pc) else p.lines.(pc)

(*****************************************************************************)
(* The calls *)
(*****************************************************************************)

type frame = { procedure : int; base : int; static_link : int; dynamic_link : int; return_address : int }

let frame_at (m : Pmachine.machine) (procedure : int) (base : int) : frame =
  { procedure; base; static_link = Pmachine.word m (base + 1); dynamic_link = Pmachine.word m (base + 2); return_address = Pmachine.word m (base + 3) }

let frames (p : Pcode.program) (m : Pmachine.machine) : frame list =
  let rec go (f : frame) = if f.procedure = 0 then [ f ] else f :: go (frame_at m (procedure_at p f.return_address) f.dynamic_link) in
  go (frame_at m (procedure_at p (Pmachine.pc m)) (Pmachine.mp m))

(*****************************************************************************)
(* Values *)
(*****************************************************************************)

let rec show (m : Pmachine.machine) (t : Pcode.vtype) (a : int) : string =
  let w = Pmachine.word m a in
  match t with
  | Vint -> string_of_int w
  | Vbool -> if w <> 0 then "TRUE" else "FALSE"
  | Vchar -> if w >= 32 && w < 127 then Printf.sprintf "'%c'" (Char.chr w) else Printf.sprintf "#%d" w
  | Varray (lo, hi, elt) ->
      let size = size_of elt in
      "(" ^ String.concat "," (List.init (hi - lo + 1) (fun i -> show m elt (a + (i * size)))) ^ ")"
  | Vrecord fields -> "(" ^ String.concat "," (List.map (fun (_, o, t) -> show m t (a + o)) fields) ^ ")"

and size_of (t : Pcode.vtype) : int =
  match t with
  | Vint | Vbool | Vchar -> 1
  | Varray (lo, hi, elt) -> (hi - lo + 1) * size_of elt
  | Vrecord fields -> List.fold_left (fun n (_, _, t) -> n + size_of t) 0 fields

(* the address and type of a variable named [n], seen from a frame:
   in its procedure, or one along the static links *)
let rec variable (p : Pcode.program) (m : Pmachine.machine) (procedure : int) (base : int) (n : string) : (int * Pcode.vtype) option =
  let q = p.procedures.(procedure) in
  match List.find_opt (fun (v : Pcode.variable) -> v.vname = n) q.variables with
  | Some v -> Some ((if v.by_ref then Pmachine.word m (base + v.offset) else base + v.offset), v.vtype)
  | None -> if q.parent < 0 then None else variable p m q.parent (Pmachine.word m (base + 1)) n

let call (p : Pcode.program) (m : Pmachine.machine) (f : frame) : string =
  let q = p.procedures.(f.procedure) in
  let params = List.filter (fun (v : Pcode.variable) -> v.param) q.variables in
  let value (v : Pcode.variable) = show m v.vtype (if v.by_ref then Pmachine.word m (f.base + v.offset) else f.base + v.offset) in
  String.uppercase_ascii q.pname ^ if params = [] then "" else "(" ^ String.concat "," (List.map value params) ^ ")"

exception Bad of string

(* x, a[i, 2], r.f: a name, then indices (numbers, or scalar variables)
   and fields *)
let watch (p : Pcode.program) (m : Pmachine.machine) (expr : string) : string =
  let f = List.hd (frames p m) in
  let s = String.lowercase_ascii (String.trim expr) in
  let n = String.length s in
  let pos = ref 0 in
  let skip () = while !pos < n && s.[!pos] = ' ' do incr pos done in
  let word () =
    skip ();
    let start = !pos in
    while !pos < n && (match s.[!pos] with 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false) do incr pos done;
    if !pos = start then raise (Bad "Syntax error") else String.sub s start (!pos - start)
  in
  let lookup name = match variable p m f.procedure f.base name with Some v -> v | None -> raise (Bad ("Unknown identifier: " ^ name)) in
  try
    let a, t = lookup (word ()) in
    let rec selectors a (t : Pcode.vtype) =
      skip ();
      if !pos >= n then (a, t)
      else if s.[!pos] = '[' then begin
        incr pos;
        let rec indices a (t : Pcode.vtype) =
          match t with
          | Varray (lo, hi, elt) ->
              let w = word () in
              let i = match int_of_string_opt w with Some i -> i | None -> (match lookup w with a', (Vint | Vchar | Vbool) -> Pmachine.word m a' | _ -> raise (Bad "Invalid index")) in
              if i < lo || i > hi then raise (Bad "Constant out of range");
              let a = a + ((i - lo) * size_of elt) in
              skip ();
              if !pos < n && s.[!pos] = ',' then (incr pos; indices a elt) else (a, elt)
          | _ -> raise (Bad "Array type required")
        in
        let a, t = indices a t in
        skip ();
        if !pos < n && s.[!pos] = ']' then incr pos else raise (Bad "']' expected");
        selectors a t
      end
      else if s.[!pos] = '.' then begin
        incr pos;
        let fld = word () in
        match t with
        | Vrecord fields -> (
            match List.find_opt (fun (n, _, _) -> n = fld) fields with Some (_, o, ft) -> selectors (a + o) ft | None -> raise (Bad ("Unknown field: " ^ fld)))
        | _ -> raise (Bad "Record type required")
      end
      else raise (Bad "Syntax error")
    in
    let a, t = selectors a t in
    show m t a
  with Bad msg -> msg

(*****************************************************************************)
(* Steps *)
(*****************************************************************************)

type step = Trace_into | Step_over | To_line of int | Continue of int list

let pause_for (p : Pcode.program) (step : step) (from : Pmachine.machine) : Pmachine.machine -> bool =
  let start_line = line p from and start_mp = Pmachine.mp from and start_count = Pmachine.executed from in
  (* whether the step has left where it started: a statement nested in
     the one we are on, on its line and in its call (the add of if k >
     1 then add(k - 1)), is the same place; a loop's body coming back to
     its line after the loop's test is not. The one mutable cell of a
     step, which the machine asks once an instruction. *)
  let left = ref false in
  fun m ->
    let pc = Pmachine.pc m in
    Pmachine.executed m > start_count
    && pc < Array.length p.statements
    && p.statements.(pc) >= 0
    &&
    let l = p.statements.(pc) and mp = Pmachine.mp m in
    if l <> start_line || mp <> start_mp then left := true;
    !left
    &&
    match step with
    | Trace_into -> true
    | Step_over -> mp <= start_mp
    | To_line target -> l = target
    | Continue breakpoints -> List.mem l breakpoints
