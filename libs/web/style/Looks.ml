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
  link_color : color;
  visited_color : color;
  base : float;
  extensions : bool;
}

let root ?(extensions = false) ~(size : float) () : t =
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
    link_color = (0, 0, 238);
    visited_color = (85, 26, 139);
    base = size;
    extensions;
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

(* HTML 3.2's align= (div's; p's and a heading's are Netscape's) *)
let align_of (l : t) (e : Dom.element) (inherited : align) : align =
  match Option.map String.lowercase_ascii (Dom.attribute ~extensions:l.extensions "align" e) with
  | Some "center" -> Center
  | Some "right" -> Right
  | Some "left" -> Left
  | _ -> inherited

let colors =
  [
    ("black", (0, 0, 0)); ("silver", (192, 192, 192)); ("gray", (128, 128, 128)); ("white", (255, 255, 255));
    ("maroon", (128, 0, 0)); ("red", (255, 0, 0)); ("purple", (128, 0, 128)); ("fuchsia", (255, 0, 255));
    ("green", (0, 128, 0)); ("lime", (0, 255, 0)); ("olive", (128, 128, 0)); ("yellow", (255, 255, 0));
    ("navy", (0, 0, 128)); ("blue", (0, 0, 255)); ("teal", (0, 128, 128)); ("aqua", (0, 255, 255));
  ]

let color_of_string (s : string) : color option =
  let s = String.lowercase_ascii (String.trim s) in
  (* the # was often left out, and browsers took the digits anyway *)
  let hex = if String.length s = 7 && s.[0] = '#' then Some (String.sub s 1 6) else if String.length s = 6 then Some s else None in
  match (List.assoc_opt s colors, hex) with
  | Some c, _ -> Some c
  | None, Some h -> (
      match int_of_string_opt ("0x" ^ h) with
      | Some n -> Some ((n lsr 16) land 255, (n lsr 8) land 255, n land 255)
      | None -> None)
  | None, None -> None

let font_scale (s : string) : float option =
  let scale = [| 0.625; 0.8125; 1.; 1.125; 1.5; 2.; 3. |] in
  let s = String.trim s in
  let n =
    if s <> "" && (s.[0] = '+' || s.[0] = '-') then
      Option.map (fun d -> 3 + d) (int_of_string_opt (if s.[0] = '+' then String.sub s 1 (String.length s - 1) else s))
    else int_of_string_opt s
  in
  Option.map (fun n -> scale.(max 1 (min 7 n) - 1)) n

(* claude: the look an extension gives, when honoured *)
let netscape (l : t) (e : Dom.element) : t =
  let color name = Option.bind (Dom.attribute ~extensions:true name e) color_of_string in
  let set c default = Option.value c ~default in
  match e.name with
  | "center" -> { l with align = Center }
  | "font" ->
      let size = Option.bind (Dom.attribute "size" e) font_scale in
      { l with size = (match size with Some k -> k *. l.base | None -> l.size); color = set (color "color") l.color }
  | "body" ->
      { l with color = set (color "text") l.color; link_color = set (color "link") l.link_color;
               visited_color = set (color "vlink") l.visited_color }
  (* a cell starts on the left, whatever it is in (<center>); a heading
   * cell is bold and centred *)
  | "td" -> { l with align = align_of l e Left }
  | "th" -> { l with bold = true; align = align_of l e Center }
  | _ -> l

let look (parent : t) (e : Dom.element) : t =
  let l = parent in
  match e.name with
  (* an extension not honoured: an unknown tag, as Mosaic had it *)
  | _ when e.origin = Netscape && not l.extensions -> l
  | _ when e.origin = Netscape -> netscape l e
  | "body" when l.extensions -> netscape l e
  | name when heading_size name <> None ->
      { l with bold = true; size = l.size *. Option.get (heading_size name); align = align_of l e l.align }
  | "b" | "strong" -> { l with bold = true }
  | "i" | "em" | "cite" | "var" | "dfn" | "address" -> { l with italic = true }
  | "tt" | "code" | "kbd" | "samp" -> { l with monospace = true }
  | "pre" | "listing" | "xmp" -> { l with monospace = true; pre = true }
  | "u" -> { l with underline = true }
  | "s" | "strike" | "del" -> { l with strike = true }
  | "big" -> { l with size = l.size *. 1.17 }
  | "small" | "sub" | "sup" -> { l with size = l.size *. 0.83 }
  | "p" | "div" -> { l with align = align_of l e l.align }
  | "a" -> (
      match Dom.attribute "href" e with
      | Some href -> { l with color = l.link_color; underline = true; link = Some href }
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
  | _ when e.origin = Netscape && not look.extensions -> none Inline
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
