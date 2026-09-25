(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Looks.mli *)

type color = int * int * int
type align = Left | Center | Right

type t = {
  size : float;
  bold : bool;
  italic : bool;
  underline : bool;
  strike : bool;
  monospace : bool;
  color : color;
  link : string option;
  pre : bool;
  align : align;
}

let root ~(size : float) : t =
  {
    size;
    bold = false;
    italic = false;
    underline = false;
    strike = false;
    monospace = false;
    color = (0, 0, 0);
    link = None;
    pre = false;
    align = Left;
  }

let leading = 1.2

(*****************************************************************************)
(* Looks: inherited *)
(*****************************************************************************)

(* a heading's size, in its parent's ems *)
let heading_size (name : string) : float option =
  match name with
  | "h1" -> Some 2.
  | "h2" -> Some 1.5
  | "h3" -> Some 1.17
  | "h4" -> Some 1.
  | "h5" -> Some 0.83
  | "h6" -> Some 0.75
  | _ -> None

(* HTML 3.2's align= *)
let align_of (e : Dom.element) (inherited : align) : align =
  match Option.map String.lowercase_ascii (Dom.attribute "align" e) with
  | Some "center" -> Center
  | Some "right" -> Right
  | Some "left" -> Left
  | _ -> inherited

let look (parent : t) (e : Dom.element) : t =
  let l = parent in
  match e.name with
  | name when heading_size name <> None ->
      { l with bold = true; size = l.size *. Option.get (heading_size name); align = align_of e l.align }
  | "b" | "strong" -> { l with bold = true }
  | "i" | "em" | "cite" | "var" | "dfn" | "address" -> { l with italic = true }
  | "tt" | "code" | "kbd" | "samp" -> { l with monospace = true }
  | "pre" | "listing" | "xmp" -> { l with monospace = true; pre = true }
  | "u" -> { l with underline = true }
  | "s" | "strike" | "del" -> { l with strike = true }
  | "big" -> { l with size = l.size *. 1.17 }
  | "small" | "sub" | "sup" -> { l with size = l.size *. 0.83 }
  | "center" -> { l with align = Center }
  | "p" | "div" -> { l with align = align_of e l.align }
  | "a" -> (
      match Dom.attribute "href" e with
      | Some href -> { l with color = (0, 0, 238); underline = true; link = Some href }
      | None -> l)
  | _ -> l

(*****************************************************************************)
(* Boxes: not inherited *)
(*****************************************************************************)

type display = Block | Inline | Rule | Hidden
type box = { display : display; margin_top : float; margin_bottom : float; indent : float; right : float }

let hidden = [ "head"; "title"; "script"; "style"; "meta"; "link"; "base" ]

let blocks =
  [
    "html"; "body"; "address"; "blockquote"; "center"; "dir"; "div"; "dl"; "dt"; "dd"; "form"; "li"; "menu"; "ol";
    "p"; "pre"; "listing"; "xmp"; "ul"; "table"; "caption"; "thead"; "tbody"; "tfoot"; "tr"; "td"; "th";
  ]

let lists = [ "ul"; "ol"; "dir"; "menu" ]

let box (look : t) (e : Dom.element) : box =
  let em k = k *. look.size in
  let block ?(indent = 0.) ?(right = 0.) top bottom =
    { display = Block; margin_top = top; margin_bottom = bottom; indent; right }
  in
  let none display = { display; margin_top = 0.; margin_bottom = 0.; indent = 0.; right = 0. } in
  match e.name with
  | name when List.mem name hidden -> none Hidden
  | "hr" -> { (none Rule) with margin_top = em 0.5; margin_bottom = em 0.5 }
  | "body" -> block ~indent:8. ~right:8. 8. 8.
  | "h1" -> block (em 0.67) (em 0.67)
  | "h2" -> block (em 0.75) (em 0.75)
  | "h3" -> block (em 0.83) (em 0.83)
  | "h4" -> block (em 1.12) (em 1.12)
  | "h5" -> block (em 1.5) (em 1.5)
  | "h6" -> block (em 1.67) (em 1.67)
  | "p" | "dl" -> block (em 1.12) (em 1.12)
  | "blockquote" -> block ~indent:40. ~right:40. (em 1.12) (em 1.12)
  | name when List.mem name lists -> block ~indent:40. (em 1.12) (em 1.12)
  | "dd" -> block ~indent:40. 0. 0.
  | "pre" | "listing" | "xmp" -> block (em 1.) (em 1.)
  | name when List.mem name blocks -> block 0. 0.
  | _ -> none Inline
