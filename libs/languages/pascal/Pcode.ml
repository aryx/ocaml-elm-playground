(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pcode.mli *)

type csp = Wri | Wrc | Wrb | Wrs of string | Wln | Rdi | Rdc | Rln | Rnd | Eol

type instr =
  | Ldc of int
  | Lod of int * int
  | Lda of int * int
  | Str of int * int
  | Ind of int
  | Sto
  | Ldm of int
  | Stm of int
  | Ixa of int
  | Inc of int
  | Chk of int * int
  | Adi | Sbi | Mpi | Dvi | Mod | Ngi | Abi | Sqi | Odd
  | Equ | Neq | Les | Leq | Grt | Geq | And | Ior | Not
  | Ujp of int
  | Fjp of int
  | Mst of int
  | Cup of int * int
  | Ent of int
  | Retp
  | Retf
  | Csp of csp
  | Stp

type program = { code : instr array; lines : int array }

let mark = 4

let show (i : instr) : string =
  let op name = Printf.sprintf "%-4s" name in
  match i with
  | Ldc n -> op "ldc" ^ string_of_int n
  | Lod (d, o) -> op "lod" ^ Printf.sprintf "%d,%d" d o
  | Lda (d, o) -> op "lda" ^ Printf.sprintf "%d,%d" d o
  | Str (d, o) -> op "str" ^ Printf.sprintf "%d,%d" d o
  | Ind o -> op "ind" ^ string_of_int o
  | Sto -> "sto"
  | Ldm n -> op "ldm" ^ string_of_int n
  | Stm n -> op "stm" ^ string_of_int n
  | Ixa n -> op "ixa" ^ string_of_int n
  | Inc n -> op "inc" ^ string_of_int n
  | Chk (lo, hi) -> op "chk" ^ Printf.sprintf "%d,%d" lo hi
  | Adi -> "adi" | Sbi -> "sbi" | Mpi -> "mpi" | Dvi -> "dvi" | Mod -> "mod" | Ngi -> "ngi"
  | Abi -> "abi" | Sqi -> "sqi" | Odd -> "odd"
  | Equ -> "equ" | Neq -> "neq" | Les -> "les" | Leq -> "leq" | Grt -> "grt" | Geq -> "geq"
  | And -> "and" | Ior -> "ior" | Not -> "not"
  | Ujp l -> op "ujp" ^ string_of_int l
  | Fjp l -> op "fjp" ^ string_of_int l
  | Mst d -> op "mst" ^ string_of_int d
  | Cup (n, l) -> op "cup" ^ Printf.sprintf "%d,%d" n l
  | Ent n -> op "ent" ^ string_of_int n
  | Retp -> "retp"
  | Retf -> "retf"
  | Csp c -> (
      op "csp"
      ^ match c with
        | Wri -> "wri" | Wrc -> "wrc" | Wrb -> "wrb" | Wrs s -> "wrs '" ^ s ^ "'" | Wln -> "wln"
        | Rdi -> "rdi" | Rdc -> "rdc" | Rln -> "rln" | Rnd -> "rnd" | Eol -> "eol")
  | Stp -> "stp"

let listing (p : program) : string =
  String.concat ""
    (Array.to_list (Array.mapi (fun a i -> Printf.sprintf "%4d  %-18s ; %d\n" a (show i) p.lines.(a)) p.code))
