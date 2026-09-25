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
  (* the # was often left out, and browsers took the digits anyway;
   * CSS's #rgb is #rrggbb, each digit twice *)
  let hex =
    match String.length s with
    | 7 when s.[0] = '#' -> Some (String.sub s 1 6)
    | 6 -> Some s
    | 4 when s.[0] = '#' -> Some (String.concat "" (List.map (fun i -> String.make 2 s.[i]) [ 1; 2; 3 ]))
    | _ -> None
  in
  (* CSS's rgb(r, g, b) *)
  let rgb =
    if String.length s > 5 && String.sub s 0 4 = "rgb(" && s.[String.length s - 1] = ')' then
      match List.map (fun x -> int_of_string_opt (String.trim x)) (String.split_on_char ',' (String.sub s 4 (String.length s - 5))) with
      | [ Some r; Some g; Some b ] -> Some (min 255 r, min 255 g, min 255 b)
      | _ -> None
    else None
  in
  match (List.assoc_opt s colors, hex, rgb) with
  | Some c, _, _ -> Some c
  | None, Some h, _ -> (
      match int_of_string_opt ("0x" ^ h) with
      | Some n -> Some ((n lsr 16) land 255, (n lsr 8) land 255, n land 255)
      | None -> None)
  | None, None, rgb -> rgb

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

type box = {
  display : display;
  margin_top : float;
  margin_bottom : float;
  indent : float;
  right : float;
  background : color option;
}

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
    { display = Block; margin_top = top; margin_bottom = bottom; indent; right; background = None }
  in
  let none display = { display; margin_top = 0.; margin_bottom = 0.; indent = 0.; right = 0.; background = None } in
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

(*****************************************************************************)
(* Style sheets: what a declaration does *)
(*****************************************************************************)

(* a length: px, em (of [em]), or a number alone (px); % of [percent] *)
let length ~(em : float) ?(percent = em) (v : string) : float option =
  let v = String.trim (String.lowercase_ascii v) in
  let number suffix =
    if String.ends_with ~suffix v then float_of_string_opt (String.trim (String.sub v 0 (String.length v - String.length suffix)))
    else None
  in
  match (number "px", number "em", number "%", number "pt") with
  | Some n, _, _, _ -> Some n
  | _, Some n, _, _ -> Some (n *. em)
  | _, _, Some n, _ -> Some (n *. percent /. 100.)
  (* a point is 1/72 inch, a pixel 1/96 *)
  | _, _, _, Some n -> Some (n *. 96. /. 72.)
  | _ -> float_of_string_opt v

(* CSS1's font-size keywords: medium the root's, a step 1.2 *)
let font_size ~(parent : t) (v : string) : float option =
  let steps = [ ("xx-small", -3); ("x-small", -2); ("small", -1); ("medium", 0); ("large", 1); ("x-large", 2); ("xx-large", 3) ] in
  match String.lowercase_ascii v with
  | k when List.mem_assoc k steps -> Some (parent.base *. (1.2 ** float_of_int (List.assoc k steps)))
  | "larger" -> Some (parent.size *. 1.2)
  | "smaller" -> Some (parent.size /. 1.2)
  | v -> length ~em:parent.size v

let styled ~(parent : t) (l : t) (declarations : (string * string) list) : t =
  List.fold_left
    (fun (l : t) (property, value) ->
      let v = String.lowercase_ascii value in
      match property with
      | "color" -> ( match color_of_string value with Some c -> { l with color = c } | None -> l)
      | "font-size" -> ( match font_size ~parent value with Some s when s > 0. -> { l with size = s } | _ -> l)
      | "font-weight" -> (
          match (v, int_of_string_opt v) with
          | ("bold" | "bolder"), _ -> { l with bold = true }
          | ("normal" | "lighter"), _ -> { l with bold = false }
          | _, Some n -> { l with bold = n >= 600 }
          | _ -> l)
      | "font-style" -> { l with italic = v = "italic" || v = "oblique" }
      | "font-family" ->
          (* one pen: all we can tell is fixed width or not *)
          { l with monospace = List.exists (fun f -> String.trim f = "monospace" || String.trim f = "courier") (String.split_on_char ',' v) }
      | "text-decoration" ->
          { l with underline = v = "underline"; strike = v = "line-through" }
      | "text-align" -> (
          match v with "center" -> { l with align = Center } | "right" -> { l with align = Right } | "left" -> { l with align = Left } | _ -> l)
      | "white-space" -> { l with pre = v = "pre" }
      | _ -> l)
    l declarations

let styled_box (l : t) (b : box) (declarations : (string * string) list) : box =
  let len v = Option.value (length ~em:l.size v) ~default:0. in
  List.fold_left
    (fun (b : box) (property, value) ->
      match (property, String.lowercase_ascii value) with
      | "display", "none" -> { b with display = Hidden }
      | "display", "block" when b.display = Inline -> { b with display = Block }
      | "display", "inline" when b.display = Block -> { b with display = Inline }
      | "margin-top", v -> { b with margin_top = len v }
      | "margin-bottom", v -> { b with margin_bottom = len v }
      | "margin-left", v -> { b with indent = len v }
      | "margin-right", v -> { b with right = len v }
      | "margin", v -> (
          (* 1 to 4 values: top, right, bottom, left, the missing ones
           * copied from their opposite *)
          match List.map len (String.split_on_char ' ' v |> List.filter (( <> ) "")) with
          | [ a ] -> { b with margin_top = a; right = a; margin_bottom = a; indent = a }
          | [ a; c ] -> { b with margin_top = a; right = c; margin_bottom = a; indent = c }
          | [ a; c; d ] -> { b with margin_top = a; right = c; margin_bottom = d; indent = c }
          | [ a; c; d; e ] -> { b with margin_top = a; right = c; margin_bottom = d; indent = e }
          | _ -> b)
      | ("background-color" | "background"), _ -> (
          match color_of_string value with Some c -> { b with background = Some c } | None -> b)
      | _ -> b)
    b declarations
