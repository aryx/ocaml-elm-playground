(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Clip *)

let t = Testo.create

(* a vertex with texture coordinate u = its x, and a normal *)
let vertex (x, y, z) : Clip.vertex = ((x, y, z), (x, 0.), (0., 0., 1.))
let positions = List.map (fun (p, _uv, _n) -> p)
let us = List.map (fun (_p, (u, _v), _n) -> u)

let vec3 =
  Alcotest.testable
    (fun ppf (x, y, z) -> Format.fprintf ppf "(%g, %g, %g)" x y z)
    (fun (a, b, c) (x, y, z) -> Float.abs (a -. x) < 1e-9 && Float.abs (b -. y) < 1e-9 && Float.abs (c -. z) < 1e-9)

let a = (0., 0., 3.)
let b = (2., 0., 3.)

(* Clip.mli's example: two vertices in front, the third behind: a quad *)
let test_quad () =
  let clipped = Clip.near_plane ~near:1. (List.map vertex [ a; b; (0., 0., -1.) ]) in
  Alcotest.(check (list vec3)) "B, (1, 0, 1), (0, 0, 1), A" [ b; (1., 0., 1.); (0., 0., 1.); a ] (positions clipped);
  Alcotest.(check (list (float 1e-9))) "u interpolated too" [ 2.; 1.; 0.; 0. ] (us clipped)

let test_triangle () =
  let clipped = Clip.near_plane ~near:1. (List.map vertex [ a; (2., 0., -1.); (-2., 0., -1.) ]) in
  Alcotest.(check (list vec3)) "one vertex in front: a smaller triangle" [ (1., 0., 1.); (-1., 0., 1.); a ]
    (positions clipped)

let test_all_or_nothing () =
  let in_front = List.map vertex [ a; b; (0., 1., 2.) ] in
  Alcotest.(check (list vec3)) "all in front: unchanged" (positions in_front)
    (positions (Clip.near_plane ~near:1. in_front));
  Alcotest.(check int) "all behind: nothing" 0
    (List.length (Clip.near_plane ~near:1. (List.map vertex [ (0., 0., -1.); (1., 0., 0.); (0., 1., 0.5) ])))

let tests =
  Testo.categorize "Clip"
    [
      t "a quad, the worked example" test_quad;
      t "a smaller triangle" test_triangle;
      t "all in front, all behind" test_all_or_nothing;
    ]
