(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Highlight_code.mli *)

type category =
  | Comment
  | Comment_section
  | Keyword
  | Keyword_control
  | Keyword_module
  | Def_function
  | Def_value
  | Def_type
  | Def_module
  | Parameter
  | Local
  | Global
  | Module
  | Constructor
  | Type
  | Type_var
  | Label
  | Capability
  | Number
  | String
  | Operator
  | Punctuation
  | Attribute
  | Normal
  | Error
  | Field

let show = function
  | Comment -> "Comment"
  | Comment_section -> "Comment_section"
  | Keyword -> "Keyword"
  | Keyword_control -> "Keyword_control"
  | Keyword_module -> "Keyword_module"
  | Def_function -> "Def_function"
  | Def_value -> "Def_value"
  | Def_type -> "Def_type"
  | Def_module -> "Def_module"
  | Parameter -> "Parameter"
  | Local -> "Local"
  | Global -> "Global"
  | Module -> "Module"
  | Constructor -> "Constructor"
  | Type -> "Type"
  | Type_var -> "Type_var"
  | Label -> "Label"
  | Capability -> "Capability"
  | Number -> "Number"
  | String -> "String"
  | Operator -> "Operator"
  | Punctuation -> "Punctuation"
  | Attribute -> "Attribute"
  | Normal -> "Normal"
  | Error -> "Error"
  | Field -> "Field"

(* claude: a new category goes at the end: a category's place here is
 * its byte in a code map's picture of a file *)
let all =
  [| Comment; Comment_section; Keyword; Keyword_control; Keyword_module; Def_function; Def_value; Def_type; Def_module;
     Parameter; Local; Global; Module; Constructor; Type; Type_var; Label; Capability; Number; String; Operator;
     Punctuation; Attribute; Normal; Error; Field |]

let index (c : category) : int =
  let rec go i = if all.(i) = c then i else go (i + 1) in
  go 0

(*****************************************************************************)
(* Colours *)
(*****************************************************************************)

(* claude: X11's names, as codemap gives them (info_of_category) *)
let rgb = function
  | Comment -> (190, 190, 190) (* gray *)
  | Comment_section -> (255, 127, 80) (* coral *)
  | Keyword -> (255, 165, 0) (* orange *)
  | Keyword_control -> (255, 140, 0) (* DarkOrange *)
  | Keyword_module -> (210, 105, 30) (* chocolate *)
  | Def_function -> (238, 201, 0) (* gold2 *)
  | Def_value -> (255, 215, 0) (* gold *)
  | Def_type -> (154, 205, 50) (* YellowGreen *)
  | Def_module -> (255, 127, 36) (* chocolate1 *)
  | Parameter -> (92, 172, 238) (* SteelBlue2 *)
  | Local -> (135, 206, 255) (* SkyBlue1 *)
  | Global -> (250, 128, 114) (* salmon *)
  | Module -> (210, 105, 30) (* chocolate *)
  | Constructor -> (255, 181, 197) (* pink1 *)
  | Type -> (127, 255, 0) (* chartreuse *)
  | Type_var -> (50, 205, 50) (* LimeGreen *)
  | Label -> (100, 149, 237) (* CornflowerBlue *)
  | Capability -> (255, 64, 64) (* ours: red, authority *)
  | Number -> (205, 205, 0) (* yellow3 *)
  | String -> (60, 179, 113) (* MediumSeaGreen *)
  | Operator -> (0, 154, 205) (* DeepSkyBlue3 *)
  | Punctuation -> (0, 205, 205) (* cyan3 *)
  | Attribute -> (238, 118, 0) (* DarkOrange2 *)
  | Normal -> (245, 222, 179) (* wheat *)
  | Error -> (255, 99, 71) (* tomato *)
  | Field -> (159, 121, 238) (* MediumPurple2, codemap's field *)

let background = (47, 79, 79) (* DarkSlateGray *)

let emphasis = function
  | Def_module | Def_type | Comment_section -> 5.
  | Def_function -> 3.5
  | Def_value -> 2.5
  | _ -> 1.

(*****************************************************************************)
(* Lines *)
(*****************************************************************************)

type span = { col : int; text : string; category : category }
type occurrence = { line : int; col : int; len : int; bound_at : int * int }

let occurrences (tokens : (int * int * string) array) (binds : (int, int) Hashtbl.t) : occurrence list =
  Hashtbl.fold
    (fun i b acc ->
      let line, col, text = tokens.(i) and bline, bcol, _ = tokens.(b) in
      { line = line - 1; col; len = String.length text; bound_at = (bline - 1, bcol) } :: acc)
    binds []

type space = Value | Type | Constr | Tag
type definition = { dname : string; dspace : space; dline : int; dcol : int; drank : int }
type reference = { rline : int; rcol : int; rlen : int; rpath : string list; rname : string; rspace : space }

type analysis = {
  spans : span list array;
  occurrences : occurrence list;
  definitions : definition list;
  references : reference list;
  opens : string list;
  includes : string list;
}

let definition (tokens : (int * int * string) array) (i : int) (space : space) (rank : int) : definition =
  let line, col, text = tokens.(i) in
  { dname = text; dspace = space; dline = line - 1; dcol = col; drank = rank }

let reference (tokens : (int * int * string) array) (i : int) (path : string list) (space : space) : reference =
  let line, col, text = tokens.(i) in
  { rline = line - 1; rcol = col; rlen = String.length text; rpath = path; rname = text; rspace = space }

let lines (src : string) (tokens : (int * int * string * category) list) : span list array =
  let nlines = List.length (String.split_on_char '\n' src) in
  let out = Array.make nlines [] in
  List.iter
    (fun (line1, col, text, category) ->
      (* a token over several lines: a span per line, the first at the
       * token's column, the others at 0 *)
      List.iteri
        (fun k piece ->
          let line = line1 - 1 + k in
          if piece <> "" && line >= 0 && line < nlines then
            out.(line) <- { col = (if k = 0 then col else 0); text = piece; category } :: out.(line))
        (String.split_on_char '\n' text))
    tokens;
  Array.map List.rev out
