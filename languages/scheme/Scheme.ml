(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scheme.mli *)

type t =
  | Int of int
  | Real of float
  | Bool of bool
  | Char of int
  | Str of string
  | Sym of string
  | Nil
  | Pair of t * t
  | Vector of t array
  | Struct of string * t list
  | Image of Scheme_image.t
  | Proc of proc
  | Void

and proc = Prim of string | Closure of lambda * env | Cont of kont | Make of string * int | Get of string * int * string | Is of string
and loc = int
and env = (string * loc) list
and expr = { desc : desc; span : Sexpr.span }

and desc =
  | Quote of t
  | Var of string
  | Lambda of lambda
  | If of expr * expr * expr
  | Set of string * expr
  | App of expr * expr list
  | Seq of expr list
  | Define of string * expr
  | Define_struct of string * string list
  | Big_bang of expr * (string * expr) list

and lambda = { params : string list; rest : string option; locals : string list; body : expr list; name : string }

and kont =
  | Halt
  | K_if of expr * expr * env * kont
  | K_app of t list * expr list * env * Sexpr.span * kont
  | K_set of loc * kont
  | K_seq of expr list * env * kont
  | K_define of string * kont
  | K_big_bang of t list * (string * expr) list * string list * env * Sexpr.span * kont

type style = Write | Constructor

(*****************************************************************************)
(* Lists *)
(*****************************************************************************)

let list (xs : t list) : t = List.fold_right (fun x rest -> Pair (x, rest)) xs Nil

let to_list (v : t) : t list option =
  let rec go v acc = match v with Nil -> Some (List.rev acc) | Pair (a, d) -> go d (a :: acc) | _ -> None in
  go v []

let truthy (v : t) : bool = v <> Bool false

(*****************************************************************************)
(* Printing *)
(*****************************************************************************)

(* 1.5, and 2.0 rather than 2: a real prints as one *)
let real (f : float) : string =
  let s = Printf.sprintf "%.15g" f in
  if String.exists (fun c -> c = '.' || c = 'e' || c = 'n' || c = 'i') s then s else s ^ ".0"

let char_name (c : int) : string =
  match c with 32 -> "space" | 10 -> "newline" | 9 -> "tab" | 0 -> "nul" | _ -> String.make 1 (Char.chr (c land 255))

let proc_name (p : proc) : string =
  match p with
  | Prim name -> name
  | Closure (l, _) -> l.name
  | Cont _ -> "continuation"
  | Make (s, _) -> "make-" ^ s
  | Get (s, _, f) -> s ^ "-" ^ f
  | Is s -> s ^ "?"

let rec print (style : style) (v : t) : string =
  let p = print style in
  let all xs = String.concat " " (List.map p xs) in
  match (style, v) with
  | _, Int n -> string_of_int n
  | _, Real f -> real f
  | Write, Bool b -> if b then "#t" else "#f"
  | Constructor, Bool b -> if b then "true" else "false"
  | _, Char c -> "#\\" ^ char_name c
  | _, Str s -> Printf.sprintf "%S" s
  | Write, Sym s -> s
  | Constructor, Sym s -> "'" ^ s
  | Write, Nil -> "()"
  | Constructor, Nil -> "empty"
  | Write, Pair _ ->
      let rec go v acc = match v with Pair (a, d) -> go d (p a :: acc) | Nil -> List.rev acc | tail -> List.rev (p tail :: "." :: acc) in
      "(" ^ String.concat " " (go v []) ^ ")"
  | Constructor, Pair (a, d) -> ( match to_list v with Some xs -> "(list " ^ all xs ^ ")" | None -> "(cons " ^ p a ^ " " ^ p d ^ ")")
  | Write, Vector xs -> "#(" ^ all (Array.to_list xs) ^ ")"
  | Constructor, Vector xs -> "(vector " ^ all (Array.to_list xs) ^ ")"
  | Write, Struct (name, fields) -> "#(struct:" ^ name ^ (if fields = [] then "" else " " ^ all fields) ^ ")"
  | Constructor, Struct (name, fields) -> "(make-" ^ name ^ (if fields = [] then "" else " " ^ all fields) ^ ")"
  | Write, Image _ -> "#<image>"
  | Constructor, Image i -> Scheme_image.to_string i
  | _, Proc (Cont _) -> "#<continuation>"
  | _, Proc (Closure ({ name = ""; _ }, _)) -> "#<procedure>"
  | _, Proc pr -> "#<procedure:" ^ proc_name pr ^ ">"
  | _, Void -> "#<void>"

let display (v : t) : string = match v with Str s -> s | Char c -> String.make 1 (Char.chr (c land 255)) | _ -> print Write v

(*****************************************************************************)
(* Equality, kinds *)
(*****************************************************************************)

let rec equal (a : t) (b : t) : bool =
  match (a, b) with
  | Pair (a1, d1), Pair (a2, d2) -> equal a1 a2 && equal d1 d2
  | Vector xs, Vector ys -> Array.length xs = Array.length ys && Array.for_all2 equal xs ys
  | Struct (n1, f1), Struct (n2, f2) -> n1 = n2 && List.length f1 = List.length f2 && List.for_all2 equal f1 f2
  (* a procedure is only itself *)
  | Proc p, Proc q -> p == q
  | _ -> a = b

let kind (v : t) : string =
  match v with
  | Int _ | Real _ -> "number"
  | Bool _ -> "boolean"
  | Char _ -> "character"
  | Str _ -> "string"
  | Sym _ -> "symbol"
  | Nil | Pair _ -> "list"
  | Vector _ -> "vector"
  | Struct (name, _) -> name
  | Image _ -> "image"
  | Proc _ -> "procedure"
  | Void -> "void"
