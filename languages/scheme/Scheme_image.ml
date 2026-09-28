(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scheme_image.mli *)

type mode = Solid | Outline

type t =
  | Circle of float * mode * string
  | Ellipse of float * float * mode * string
  | Rectangle of float * float * mode * string
  | Triangle of float * mode * string
  | Text of string * float * string
  | Scene of float * float
  | Beside of t * t
  | Above of t * t
  | Overlay of t * t
  | Place of t * float * float * t

let rec width (i : t) : float =
  match i with
  | Circle (r, _, _) -> 2. *. r
  | Ellipse (w, _, _, _) | Rectangle (w, _, _, _) | Scene (w, _) -> w
  | Triangle (s, _, _) -> s
  | Text (s, size, _) -> 0.6 *. size *. float_of_int (String.length s)
  | Beside (a, b) -> width a +. width b
  | Above (a, b) | Overlay (a, b) -> Float.max (width a) (width b)
  | Place (_, _, _, scene) -> width scene

and height (i : t) : float =
  match i with
  | Circle (r, _, _) -> 2. *. r
  | Ellipse (_, h, _, _) | Rectangle (_, h, _, _) | Scene (_, h) -> h
  (* an equilateral triangle's height: its side times sqrt 3 / 2 *)
  | Triangle (s, _, _) -> s *. sqrt 3. /. 2.
  | Text (_, size, _) -> size
  | Above (a, b) -> height a +. height b
  | Beside (a, b) | Overlay (a, b) -> Float.max (height a) (height b)
  | Place (_, _, _, scene) -> height scene

let rec to_string (i : t) : string =
  let n f = if Float.is_integer f then string_of_int (int_of_float f) else Printf.sprintf "%g" f in
  let m = function Solid -> "\"solid\"" | Outline -> "\"outline\"" in
  match i with
  | Circle (r, md, c) -> Printf.sprintf "(circle %s %s %S)" (n r) (m md) c
  | Ellipse (w, h, md, c) -> Printf.sprintf "(ellipse %s %s %s %S)" (n w) (n h) (m md) c
  | Rectangle (w, h, md, c) -> Printf.sprintf "(rectangle %s %s %s %S)" (n w) (n h) (m md) c
  | Triangle (s, md, c) -> Printf.sprintf "(triangle %s %s %S)" (n s) (m md) c
  | Text (s, size, c) -> Printf.sprintf "(text %S %s %S)" s (n size) c
  | Scene (w, h) -> Printf.sprintf "(empty-scene %s %s)" (n w) (n h)
  | Beside (a, b) -> Printf.sprintf "(beside %s %s)" (to_string a) (to_string b)
  | Above (a, b) -> Printf.sprintf "(above %s %s)" (to_string a) (to_string b)
  | Overlay (a, b) -> Printf.sprintf "(overlay %s %s)" (to_string a) (to_string b)
  | Place (a, x, y, s) -> Printf.sprintf "(place-image %s %s %s %s)" (to_string a) (n x) (n y) (to_string s)
