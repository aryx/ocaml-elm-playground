(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Css_values.mli *)
open Css_syntax

type length = { px : float; pct : float }
type color = { r : int; g : int; b : int; a : float }
type context = { em : float; rem : float; viewport_width : float; viewport_height : float }

let resolve (l : length) (base : float) : float = l.px +. (l.pct *. base /. 100.)
let px (p : float) : length = { px = p; pct = 0. }
let zero = px 0.
let black = { r = 0; g = 0; b = 0; a = 1. }
let transparent = { r = 0; g = 0; b = 0; a = 0. }

let parts (cs : component list) : component list = List.filter (fun c -> c <> Token Whitespace) cs

(*****************************************************************************)
(* var() *)
(*****************************************************************************)

exception Unresolved

let substitute (lookup : string -> component list option) (cs : component list) : component list option =
  let rec go (depth : int) (cs : component list) : component list =
    if depth > 32 then raise Unresolved (* a cycle: --a: var(--a) *)
    else
      List.concat_map
        (fun (c : component) ->
          match c with
          | Func (f, args) when String.lowercase_ascii f = "var" -> (
              let name, fallback =
                match split_on Comma args with
                | first :: rest -> (to_string (trim first), if rest = [] then None else Some (trim (List.concat (List.mapi (fun i r -> if i = 0 then r else Token Comma :: r) rest))))
                | [] -> raise Unresolved
              in
              match (lookup name, fallback) with
              | Some v, _ -> go (depth + 1) v
              | None, Some f -> go (depth + 1) f
              | None, None -> raise Unresolved)
          | Func (f, args) -> [ Func (f, go depth args) ]
          | Block (o, inside) -> [ Block (o, go depth inside) ]
          | c -> [ c ])
        cs
  in
  match go 0 cs with v -> Some v | exception Unresolved -> None

(*****************************************************************************)
(* Lengths and calc() *)
(*****************************************************************************)

let add a b = { px = a.px +. b.px; pct = a.pct +. b.pct }
let scale k a = { px = k *. a.px; pct = k *. a.pct }

let unit_length (ctx : context) (f : float) (u : string) : length option =
  match u with
  | "px" -> Some (px f)
  | "em" -> Some (px (f *. ctx.em))
  | "rem" -> Some (px (f *. ctx.rem))
  | "ex" | "ch" -> Some (px (f *. ctx.em *. 0.5))
  | "pt" -> Some (px (f *. 96. /. 72.))
  | "pc" -> Some (px (f *. 16.))
  | "in" -> Some (px (f *. 96.))
  | "cm" -> Some (px (f *. 96. /. 2.54))
  | "mm" -> Some (px (f *. 96. /. 25.4))
  | "vw" -> Some (px (f *. ctx.viewport_width /. 100.))
  | "vh" -> Some (px (f *. ctx.viewport_height /. 100.))
  | "vmin" -> Some (px (f *. Float.min ctx.viewport_width ctx.viewport_height /. 100.))
  | "vmax" -> Some (px (f *. Float.max ctx.viewport_width ctx.viewport_height /. 100.))
  | _ -> None

(* calc()'s expression: a sum of products, a number or a length each
 * term (a number alone multiplies or divides a length) *)
type term = Num of float | Len of length

let rec length (ctx : context) (c : component) : length option =
  match c with
  | Token (Dimension (f, u)) -> unit_length ctx f u
  | Token (Percentage f) -> Some { px = 0.; pct = f }
  | Token (Number 0.) -> Some zero
  | Func (f, args) -> (
      match String.lowercase_ascii f with
      | "calc" -> ( match calc ctx args with Some (Len l) -> Some l | Some (Num 0.) -> Some zero | _ -> None)
      | ("min" | "max" | "clamp") as f -> (
          let ls = List.map (fun a -> calc ctx a) (split_on Comma args) in
          if List.exists (fun l -> match l with Some (Len _) -> false | _ -> true) ls then None
          else
            let ls = List.map (function Some (Len l) -> l | _ -> zero) ls in
            let pick cmp = List.fold_left (fun a b -> if cmp b.px a.px then b else a) (List.hd ls) ls in
            match (f, ls) with
            | "min", _ :: _ -> Some (pick ( < ))
            | "max", _ :: _ -> Some (pick ( > ))
            | "clamp", [ lo; v; hi ] -> Some (if v.px < lo.px then lo else if v.px > hi.px then hi else v)
            | _ -> None)
      | _ -> None)
  | Block ('(', inside) -> ( match calc ctx inside with Some (Len l) -> Some l | _ -> None)
  | _ -> None

and calc (ctx : context) (cs : component list) : term option =
  let cs = parts cs in
  let operand (c : component) : term option =
    match c with
    | Token (Number f) -> Some (Num f)
    | Block ('(', inside) -> calc ctx inside
    | c -> Option.map (fun l -> Len l) (length ctx c)
  in
  (* the products first, then the sums: a precedence of two levels *)
  let times (a : term) (op : char) (b : term) : term option =
    match (a, op, b) with
    | Num x, '*', Num y -> Some (Num (x *. y))
    | Num x, '*', Len l | Len l, '*', Num x -> Some (Len (scale x l))
    | Num x, '/', Num y when y <> 0. -> Some (Num (x /. y))
    | Len l, '/', Num y when y <> 0. -> Some (Len (scale (1. /. y) l))
    | _ -> None
  in
  let plus (a : term) (sign : float) (b : term) : term option =
    match (a, b) with
    | Num x, Num y -> Some (Num (x +. (sign *. y)))
    | Len x, Len y -> Some (Len (add x (scale sign y)))
    | Num 0., Len y -> Some (Len (scale sign y))
    | Len x, Num 0. -> Some (Len x)
    | _ -> None
  in
  let rec product (acc : term) (cs : component list) : (term * component list) option =
    match cs with
    | Token (Delim (('*' | '/') as op)) :: c :: rest -> (
        match operand c with Some b -> Option.bind (times acc op b) (fun t -> product t rest) | None -> None)
    | rest -> Some (acc, rest)
  in
  let rec sum (acc : term) (cs : component list) : term option =
    match cs with
    | [] -> Some acc
    | Token (Delim (('+' | '-') as op)) :: c :: rest -> (
        match operand c with
        | Some b -> (
            match product b rest with
            | Some (b, rest) -> Option.bind (plus acc (if op = '+' then 1. else -1.) b) (fun t -> sum t rest)
            | None -> None)
        | None -> None)
    (* "- 2em" read by the tokenizer as a negative number or dimension *)
    | (Token (Number f) as c) :: rest when f < 0. -> sum acc (Token (Delim '+') :: c :: rest)
    | (Token (Dimension (f, _)) as c) :: rest when f < 0. -> sum acc (Token (Delim '+') :: c :: rest)
    | _ -> None
  in
  match cs with
  | c :: rest -> ( match operand c with Some t -> Option.bind (product t rest) (fun (t, rest) -> sum t rest) | None -> None)
  | [] -> None

(*****************************************************************************)
(* Colours *)
(*****************************************************************************)

let named =
  [ ("aliceblue", 0xf0f8ff); ("antiquewhite", 0xfaebd7); ("aqua", 0x00ffff); ("aquamarine", 0x7fffd4); ("azure", 0xf0ffff);
    ("beige", 0xf5f5dc); ("bisque", 0xffe4c4); ("black", 0x000000); ("blanchedalmond", 0xffebcd); ("blue", 0x0000ff);
    ("blueviolet", 0x8a2be2); ("brown", 0xa52a2a); ("burlywood", 0xdeb887); ("cadetblue", 0x5f9ea0); ("chartreuse", 0x7fff00);
    ("chocolate", 0xd2691e); ("coral", 0xff7f50); ("cornflowerblue", 0x6495ed); ("cornsilk", 0xfff8dc); ("crimson", 0xdc143c);
    ("cyan", 0x00ffff); ("darkblue", 0x00008b); ("darkcyan", 0x008b8b); ("darkgoldenrod", 0xb8860b); ("darkgray", 0xa9a9a9);
    ("darkgreen", 0x006400); ("darkgrey", 0xa9a9a9); ("darkkhaki", 0xbdb76b); ("darkmagenta", 0x8b008b);
    ("darkolivegreen", 0x556b2f); ("darkorange", 0xff8c00); ("darkorchid", 0x9932cc); ("darkred", 0x8b0000);
    ("darksalmon", 0xe9967a); ("darkseagreen", 0x8fbc8f); ("darkslateblue", 0x483d8b); ("darkslategray", 0x2f4f4f);
    ("darkslategrey", 0x2f4f4f); ("darkturquoise", 0x00ced1); ("darkviolet", 0x9400d3); ("deeppink", 0xff1493);
    ("deepskyblue", 0x00bfff); ("dimgray", 0x696969); ("dimgrey", 0x696969); ("dodgerblue", 0x1e90ff); ("firebrick", 0xb22222);
    ("floralwhite", 0xfffaf0); ("forestgreen", 0x228b22); ("fuchsia", 0xff00ff); ("gainsboro", 0xdcdcdc); ("ghostwhite", 0xf8f8ff);
    ("gold", 0xffd700); ("goldenrod", 0xdaa520); ("gray", 0x808080); ("green", 0x008000); ("greenyellow", 0xadff2f);
    ("grey", 0x808080); ("honeydew", 0xf0fff0); ("hotpink", 0xff69b4); ("indianred", 0xcd5c5c); ("indigo", 0x4b0082);
    ("ivory", 0xfffff0); ("khaki", 0xf0e68c); ("lavender", 0xe6e6fa); ("lavenderblush", 0xfff0f5); ("lawngreen", 0x7cfc00);
    ("lemonchiffon", 0xfffacd); ("lightblue", 0xadd8e6); ("lightcoral", 0xf08080); ("lightcyan", 0xe0ffff);
    ("lightgoldenrodyellow", 0xfafad2); ("lightgray", 0xd3d3d3); ("lightgreen", 0x90ee90); ("lightgrey", 0xd3d3d3);
    ("lightpink", 0xffb6c1); ("lightsalmon", 0xffa07a); ("lightseagreen", 0x20b2aa); ("lightskyblue", 0x87cefa);
    ("lightslategray", 0x778899); ("lightslategrey", 0x778899); ("lightsteelblue", 0xb0c4de); ("lightyellow", 0xffffe0);
    ("lime", 0x00ff00); ("limegreen", 0x32cd32); ("linen", 0xfaf0e6); ("magenta", 0xff00ff); ("maroon", 0x800000);
    ("mediumaquamarine", 0x66cdaa); ("mediumblue", 0x0000cd); ("mediumorchid", 0xba55d3); ("mediumpurple", 0x9370db);
    ("mediumseagreen", 0x3cb371); ("mediumslateblue", 0x7b68ee); ("mediumspringgreen", 0x00fa9a);
    ("mediumturquoise", 0x48d1cc); ("mediumvioletred", 0xc71585); ("midnightblue", 0x191970); ("mintcream", 0xf5fffa);
    ("mistyrose", 0xffe4e1); ("moccasin", 0xffe4b5); ("navajowhite", 0xffdead); ("navy", 0x000080); ("oldlace", 0xfdf5e6);
    ("olive", 0x808000); ("olivedrab", 0x6b8e23); ("orange", 0xffa500); ("orangered", 0xff4500); ("orchid", 0xda70d6);
    ("palegoldenrod", 0xeee8aa); ("palegreen", 0x98fb98); ("paleturquoise", 0xafeeee); ("palevioletred", 0xdb7093);
    ("papayawhip", 0xffefd5); ("peachpuff", 0xffdab9); ("peru", 0xcd853f); ("pink", 0xffc0cb); ("plum", 0xdda0dd);
    ("powderblue", 0xb0e0e6); ("purple", 0x800080); ("rebeccapurple", 0x663399); ("red", 0xff0000); ("rosybrown", 0xbc8f8f);
    ("royalblue", 0x4169e1); ("saddlebrown", 0x8b4513); ("salmon", 0xfa8072); ("sandybrown", 0xf4a460); ("seagreen", 0x2e8b57);
    ("seashell", 0xfff5ee); ("sienna", 0xa0522d); ("silver", 0xc0c0c0); ("skyblue", 0x87ceeb); ("slateblue", 0x6a5acd);
    ("slategray", 0x708090); ("slategrey", 0x708090); ("snow", 0xfffafa); ("springgreen", 0x00ff7f); ("steelblue", 0x4682b4);
    ("tan", 0xd2b48c); ("teal", 0x008080); ("thistle", 0xd8bfd8); ("tomato", 0xff6347); ("turquoise", 0x40e0d0);
    ("violet", 0xee82ee); ("wheat", 0xf5deb3); ("white", 0xffffff); ("whitesmoke", 0xf5f5f5); ("yellow", 0xffff00);
    ("yellowgreen", 0x9acd32) ]

let of_rgb (n : int) : color = { r = (n lsr 16) land 255; g = (n lsr 8) land 255; b = n land 255; a = 1. }

let hex (h : string) : color option =
  let d i = int_of_string_opt ("0x" ^ String.make 2 h.[i]) and dd i = int_of_string_opt ("0x" ^ String.sub h i 2) in
  let mk r g b a = match (r, g, b, a) with Some r, Some g, Some b, Some a -> Some { r; g; b; a = float_of_int a /. 255. } | _ -> None in
  match String.length h with
  | 3 -> mk (d 0) (d 1) (d 2) (Some 255)
  | 4 -> mk (d 0) (d 1) (d 2) (d 3)
  | 6 -> mk (dd 0) (dd 2) (dd 4) (Some 255)
  | 8 -> mk (dd 0) (dd 2) (dd 4) (dd 6)
  | _ -> None

let clamp255 (f : float) : int = max 0 (min 255 (int_of_float (Float.round f)))

(* hsl to rgb: CSS Color's own algorithm *)
let hsl (h : float) (s : float) (l : float) : int * int * int =
  let h = Float.rem (Float.rem h 360. +. 360.) 360. /. 360. in
  let hue m1 m2 h =
    let h = if h < 0. then h +. 1. else if h > 1. then h -. 1. else h in
    if h *. 6. < 1. then m1 +. ((m2 -. m1) *. h *. 6.) else if h *. 2. < 1. then m2 else if h *. 3. < 2. then m1 +. ((m2 -. m1) *. ((2. /. 3.) -. h) *. 6.) else m1
  in
  let m2 = if l <= 0.5 then l *. (s +. 1.) else l +. s -. (l *. s) in
  let m1 = (l *. 2.) -. m2 in
  (clamp255 (255. *. hue m1 m2 (h +. (1. /. 3.))), clamp255 (255. *. hue m1 m2 h), clamp255 (255. *. hue m1 m2 (h -. (1. /. 3.))))

let color ~(current : color) (cs : component list) : color option =
  match parts cs with
  | [ Token (Hash h) ] -> hex h
  | [ Token (Ident n) ] -> (
      match String.lowercase_ascii n with
      | "transparent" -> Some transparent
      | "currentcolor" -> Some current
      | n -> Option.map of_rgb (List.assoc_opt n named))
  | [ Func (f, args) ] -> (
      (* the arguments, commas or not, and an alpha after a / or a fourth *)
      let items = List.filter (fun c -> match c with Token Comma | Token (Delim '/') -> false | _ -> true) (parts args) in
      let num c = match c with Token (Number n) -> Some n | Token (Percentage p) -> Some (p *. 2.55) | _ -> None in
      let alpha c = match c with Token (Number n) -> Some n | Token (Percentage p) -> Some (p /. 100.) | _ -> None in
      let pct c = match c with Token (Percentage p) -> Some (p /. 100.) | Token (Number n) -> Some (n /. 100.) | _ -> None in
      let deg c = match c with Token (Number n) -> Some n | Token (Dimension (n, "deg")) -> Some n | Token (Dimension (n, "turn")) -> Some (n *. 360.) | _ -> None in
      match (String.lowercase_ascii f, items) with
      | ("rgb" | "rgba"), [ r; g; b ] -> ( match (num r, num g, num b) with Some r, Some g, Some b -> Some { r = clamp255 r; g = clamp255 g; b = clamp255 b; a = 1. } | _ -> None)
      | ("rgb" | "rgba"), [ r; g; b; a ] -> (
          match (num r, num g, num b, alpha a) with Some r, Some g, Some b, Some a -> Some { r = clamp255 r; g = clamp255 g; b = clamp255 b; a } | _ -> None)
      | ("hsl" | "hsla"), h :: s :: l :: rest -> (
          match (deg h, pct s, pct l) with
          | Some h, Some s, Some l ->
              let r, g, b = hsl h s l in
              let a = match rest with [ a ] -> Option.value (alpha a) ~default:1. | _ -> 1. in
              Some { r; g; b; a }
          | _ -> None)
      | _ -> None)
  | _ -> None
