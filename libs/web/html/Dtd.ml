(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Dtd.mli *)

let void = [ "area"; "base"; "basefont"; "br"; "col"; "embed"; "frame"; "hr"; "img"; "input"; "isindex"; "link"; "meta"; "param"; "wbr" ]
let head = [ "title"; "meta"; "link"; "base"; "style"; "script" ]
let headings = [ "h1"; "h2"; "h3"; "h4"; "h5"; "h6" ]

let blocks =
  [ "address"; "blockquote"; "center"; "dir"; "div"; "dl"; "form"; "hr"; "menu"; "ol"; "p"; "pre"; "table"; "ul" ]
  @ headings

(* where no search goes through: the table's cells and the root (the
 * WHATWG's "scope") *)
let scope = [ "html"; "table"; "td"; "th"; "caption"; "applet"; "object" ]

let is_void name = List.mem name void
let is_head_element name = List.mem name head
let is_block name = List.mem name blocks

let closes (x : string) (y : string) : bool =
  match y with
  | "p" -> is_block x || List.mem x [ "li"; "dt"; "dd" ]
  | "li" -> x = "li"
  | "dt" | "dd" -> x = "dt" || x = "dd"
  | "option" -> x = "option"
  | "tr" -> x = "tr"
  | "td" | "th" -> List.mem x [ "td"; "th"; "tr" ]
  | y when List.mem y headings -> List.mem x headings
  | _ -> false

let stops (x : string) (y : string) : bool =
  List.mem y scope
  ||
  match x with
  (* an item ends the item of its own list only: not across a nested
   * list, nor across a block (other than the p, div, address an item
   * may hold) *)
  | "li" -> List.mem y [ "ul"; "ol"; "dir"; "menu" ] || (is_block y && not (List.mem y [ "p"; "div"; "address" ]))
  | "dt" | "dd" -> y = "dl" || (is_block y && not (List.mem y [ "p"; "div"; "address" ]))
  | _ -> false
