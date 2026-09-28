(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Token_ml.mli *)

type kind =
  | Comment
  | Keyword
  | Lident
  | Uident
  | Label
  | Type_var
  | Int
  | Float
  | Char
  | String
  | Operator
  | Punctuation
  | Directive
  | Error

type t = { kind : kind; text : string; offset : int; line : int; col : int }

let show_kind = function
  | Comment -> "Comment"
  | Keyword -> "Keyword"
  | Lident -> "Lident"
  | Uident -> "Uident"
  | Label -> "Label"
  | Type_var -> "Type_var"
  | Int -> "Int"
  | Float -> "Float"
  | Char -> "Char"
  | String -> "String"
  | Operator -> "Operator"
  | Punctuation -> "Punctuation"
  | Directive -> "Directive"
  | Error -> "Error"
