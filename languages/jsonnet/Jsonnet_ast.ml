(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Jsonnet_ast.mli *)

type visibility = Default | Hidden | Visible

type expr =
  | Null
  | Bool of bool
  | Num of float
  | Str of string
  | Self
  | Dollar
  | Var of string
  | Array of expr list
  | Array_comp of expr * comp list
  | Object of member list
  | Object_comp of (string * expr) list * expr * expr * comp list
  | Field of expr * string
  | Index of expr * expr
  | Slice of expr * expr option * expr option * expr option
  | Super_field of string
  | Super_index of expr
  | In_super of expr
  | Call of expr * expr list * (string * expr) list
  | Local of (string * expr) list * expr
  | If of expr * expr * expr option
  | Binary of string * expr * expr
  | Unary of string * expr
  | Function of param list * expr
  | Assert of expr * expr option * expr
  | Import of string
  | Importstr of string
  | Error of expr
  | At of int * expr

and param = string * expr option
and comp = For of string * expr | If_comp of expr

and member =
  | Field_m of field_name * bool * visibility * expr
  | Local_m of string * expr
  | Assert_m of expr * expr option

and field_name = Fixed of string | Computed of expr
