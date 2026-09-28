(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_bytecode.mli *)

module M = St_memory

type oop = M.oop

let special_selectors =
  [|
    "+"; "-"; "<"; ">"; "<="; ">="; "="; "~="; "*"; "/"; "\\\\"; "@"; "bitShift:"; "//"; "bitAnd:"; "bitOr:";
    "at:"; "at:put:"; "size"; "next"; "nextPut:"; "atEnd"; "=="; "class"; "blockCopy:"; "value"; "value:"; "do:";
    "new"; "new:"; "x"; "y";
  |]

(*****************************************************************************)
(* The header *)
(*****************************************************************************)

type header = { primitive : int; num_args : int; num_temps : int; frame_size : int }

(* primitive: 9 bits, arguments: 5, temporaries: 6, frame: 8 -- 28
 * bits, a SmallInteger *)
let encode_header (h : header) : int =
  h.primitive lor (h.num_args lsl 9) lor (h.num_temps lsl 14) lor (h.frame_size lsl 20)

let decode_header (i : int) : header =
  { primitive = i land 511; num_args = (i lsr 9) land 31; num_temps = (i lsr 14) land 63; frame_size = (i lsr 20) land 255 }

(*****************************************************************************)
(* A CompiledMethod *)
(*****************************************************************************)

let trailer_size = 5

let new_method (m : M.t) ~(header : header) ~(literals : oop array) ~(bytecodes : Bytes.t) ~(selector : oop) ~(cls : oop)
    ~(source : string) ~(pcmap : (int * int * int) list) ~(temp_names : string list) : oop =
  let src = M.new_string m source in
  let map = M.new_array m (Array.of_list (List.concat_map (fun (pc, a, b) -> [ M.of_int pc; M.of_int a; M.of_int b ]) pcmap)) in
  let names = M.new_array m (Array.of_list (List.map (M.new_string m) temp_names)) in
  let fields = Array.concat [ [| M.of_int (encode_header header) |]; literals; [| selector; cls; src; map; names |] ] in
  M.alloc m ~cls:(M.known m).compiled_method (M.Method (fields, bytecodes))

let fields (m : M.t) (meth : oop) : oop array = match M.body m meth with M.Method (f, _) -> f | _ -> [||]
let header (m : M.t) (meth : oop) : header = decode_header (M.int_of (fields m meth).(0))

let literals (m : M.t) (meth : oop) : oop array =
  let f = fields m meth in
  Array.sub f 1 (Array.length f - 1 - trailer_size)

let literal (m : M.t) (meth : oop) (i : int) : oop = (fields m meth).(i + 1)
let bytecodes (m : M.t) (meth : oop) : Bytes.t = match M.body m meth with M.Method (_, b) -> b | _ -> Bytes.empty

let trailer (m : M.t) (meth : oop) (k : int) : oop =
  let f = fields m meth in
  f.(Array.length f - trailer_size + k)

let selector m meth = trailer m meth 0
let method_class m meth = trailer m meth 1
let source m meth = M.string_of m (trailer m meth 2)

let temp_names (m : M.t) (meth : oop) : string list = Array.to_list (Array.map (M.string_of m) (M.fields m (trailer m meth 4)))

let pcmap (m : M.t) (meth : oop) : (int * int * int) list =
  let a = M.fields m (trailer m meth 3) in
  List.init (Array.length a / 3) (fun i -> (M.int_of a.(3 * i), M.int_of a.((3 * i) + 1), M.int_of a.((3 * i) + 2)))

(*****************************************************************************)
(* Reading them *)
(*****************************************************************************)

let length_at (b : Bytes.t) (pc : int) : int =
  match Char.code (Bytes.get b pc) with
  | 128 | 129 | 130 | 131 | 133 -> 2
  | 132 | 134 -> 3
  | c when c >= 160 && c <= 175 -> 2
  | _ -> 1

let is_send (b : Bytes.t) (pc : int) : bool =
  match Char.code (Bytes.get b pc) with 131 | 132 | 133 | 134 -> true | c -> c >= 176

let disassemble ~(show_literal : int -> string) (b : Bytes.t) : (int * string) list =
  let n = Bytes.length b in
  let byte i = Char.code (Bytes.get b i) in
  let rec go pc acc =
    if pc >= n then List.rev acc
    else
      let c = byte pc in
      let len = length_at b pc in
      let raw = String.concat " " (List.init len (fun i -> string_of_int (byte (pc + i)))) in
      let ext () = byte (pc + 1) in
      let kind j = match j with 0 -> "receiver variable" | 1 -> "temporary" | 2 -> "literal" | _ -> "literal variable" in
      let what =
        match c with
        | c when c <= 15 -> Printf.sprintf "push receiver variable %d" c
        | c when c <= 31 -> Printf.sprintf "push temporary %d" (c - 16)
        | c when c <= 63 -> Printf.sprintf "push %s" (show_literal (c - 32))
        | c when c <= 95 -> Printf.sprintf "push %s" (show_literal (c - 64))
        | c when c <= 103 -> Printf.sprintf "pop into receiver variable %d" (c - 96)
        | c when c <= 111 -> Printf.sprintf "pop into temporary %d" (c - 104)
        | c when c <= 119 -> "push " ^ [| "self"; "true"; "false"; "nil"; "-1"; "0"; "1"; "2" |].(c - 112)
        | c when c <= 123 -> "return " ^ [| "self"; "true"; "false"; "nil" |].(c - 120)
        | 124 -> "return top"
        | 125 -> "block return top"
        | 128 -> Printf.sprintf "push %s %d" (kind (ext () lsr 6)) (ext () land 63)
        | 129 -> Printf.sprintf "store into %s %d" (kind (ext () lsr 6)) (ext () land 63)
        | 130 -> Printf.sprintf "pop into %s %d" (kind (ext () lsr 6)) (ext () land 63)
        | 131 -> Printf.sprintf "send %s" (show_literal (ext () land 31))
        | 132 -> Printf.sprintf "send %s" (show_literal (byte (pc + 2)))
        | 133 -> Printf.sprintf "send to super %s" (show_literal (ext () land 31))
        | 134 -> Printf.sprintf "send to super %s" (show_literal (byte (pc + 2)))
        | 135 -> "pop"
        | 136 -> "dup"
        | 137 -> "push thisContext"
        | c when c >= 144 && c <= 151 -> Printf.sprintf "jump to %d" (pc + 1 + (c - 143))
        | c when c >= 152 && c <= 159 -> Printf.sprintf "jump on false to %d" (pc + 1 + (c - 151))
        | c when c >= 160 && c <= 167 -> Printf.sprintf "jump to %d" (pc + 2 + (((c - 164) * 256) + ext ()))
        | c when c >= 168 && c <= 171 -> Printf.sprintf "jump on true to %d" (pc + 2 + (((c - 168) * 256) + ext ()))
        | c when c >= 172 && c <= 175 -> Printf.sprintf "jump on false to %d" (pc + 2 + (((c - 172) * 256) + ext ()))
        | c when c >= 176 && c <= 207 -> "send " ^ special_selectors.(c - 176)
        | c when c >= 208 -> Printf.sprintf "send %s" (show_literal (c land 15))
        | _ -> "?"
      in
      go (pc + len) ((pc, Printf.sprintf "%-9s %s" raw what) :: acc)
  in
  go 0 []
