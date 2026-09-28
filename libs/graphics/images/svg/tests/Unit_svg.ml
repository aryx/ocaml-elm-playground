(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_svg.mli *)

let picture ?color (text : string) (w : int) (h : int) : Rgba_image.t =
  match Svg.parse text with Some nd -> Svg.render ?color nd ~width:w ~height:h | None -> Alcotest.fail "no <svg>"

(* a pixel's red, green, blue, alpha *)
let pixel (img : Rgba_image.t) (x : int) (y : int) : int list =
  let i = 4 * ((y * img.width) + x) in
  [ img.rgba.{i}; img.rgba.{i + 1}; img.rgba.{i + 2}; img.rgba.{i + 3} ]

let rgba = Alcotest.(list int)
let alpha img x y = List.nth (pixel img x y) 3

let tests =
  Testo.categorize "Svg"
    [
      Testo.create "the worked example: a red square" (fun () ->
          let img = picture {|<svg width="4" height="4"><rect x="1" y="1" width="2" height="2" fill="red"/></svg>|} 4 4 in
          Alcotest.check rgba "inside: opaque red" [ 255; 0; 0; 255 ] (pixel img 1 1);
          Alcotest.check rgba "inside too" [ 255; 0; 0; 255 ] (pixel img 2 2);
          Alcotest.check rgba "outside: transparent" [ 0; 0; 0; 0 ] (pixel img 0 0));
      Testo.create "the viewBox scales, relative commands" (fun () ->
          (* a unit square in a 2 by 2 viewBox, drawn 4 by 4: 2 by 2 *)
          let img = picture {|<svg viewBox="0 0 2 2"><path d="m0 0h1v1h-1z"/></svg>|} 4 4 in
          Alcotest.(check (list int)) "its four pixels black" [ 255; 255; 255; 255 ] [ alpha img 0 0; alpha img 1 0; alpha img 0 1; alpha img 1 1 ];
          Alcotest.check Alcotest.int "outside" 0 (alpha img 3 3));
      Testo.create "numbers run together" (fun () ->
          (* "M0 0L4-0 4 4-0 4z": the linetos' pairs, a sign starting the next number *)
          let img = picture {|<svg width="4" height="4"><path d="M0,0L4-0 4 4-0 4z"/></svg>|} 4 4 in
          Alcotest.check Alcotest.int "all filled" 255 (alpha img 3 3));
      Testo.create "an arc: two half circles" (fun () ->
          let img = picture {|<svg width="10" height="10"><path d="M0 5A5 5 0 1 0 10 5A5 5 0 1 0 0 5Z" fill="blue"/></svg>|} 10 10 in
          Alcotest.check rgba "the centre" [ 0; 0; 255; 255 ] (pixel img 5 5);
          Alcotest.check Alcotest.int "a corner, outside the circle" 0 (alpha img 0 0));
      Testo.create "evenodd: a hole" (fun () ->
          let square a b = Printf.sprintf "M%d %dH%dV%dH%dZ" a a b b a in
          let d = square 0 6 ^ square 2 4 in
          let img rule = picture (Printf.sprintf {|<svg width="6" height="6"><path fill-rule="%s" d="%s"/></svg>|} rule d) 6 6 in
          Alcotest.check Alcotest.int "evenodd: the middle empty" 0 (alpha (img "evenodd") 3 3);
          Alcotest.check Alcotest.int "nonzero: filled" 255 (alpha (img "nonzero") 3 3));
      Testo.create "currentColor, style=, stroke" (fun () ->
          let img = picture ~color:(0, 128, 0) {|<svg width="4" height="4"><rect width="4" height="4" style="fill: currentColor"/></svg>|} 4 4 in
          Alcotest.check rgba "the caller's colour" [ 0; 128; 0; 255 ] (pixel img 2 2);
          let img = picture {|<svg width="10" height="10"><line x1="0" y1="5" x2="10" y2="5" stroke="#000" stroke-width="2"/></svg>|} 10 10 in
          Alcotest.check Alcotest.bool "the line drawn" true (alpha img 5 5 > 200);
          Alcotest.check Alcotest.int "away from it" 0 (alpha img 5 1));
      Testo.create "reading: XML's odds and ends, sniffing, size" (fun () ->
          let text = {|<?xml version="1.0"?><!-- a logo --><!DOCTYPE svg><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 12" width="48"><g><path d="M0 0"/></g></svg>|} in
          Alcotest.(check bool) "sniffed" true (Svg.sniff text);
          Alcotest.(check bool) "HTML is not" false (Svg.sniff "<html><svg></svg>");
          match Svg.parse text with
          | Some nd ->
              Alcotest.(check (option (pair (float 1e-6) (float 1e-6)))) "width 48, the viewBox's ratio" (Some (48., 24.)) (Svg.size nd);
              Alcotest.(check (list string)) "its child" [ "g" ] (List.map (fun (c : Svg.node) -> c.name) nd.children)
          | None -> Alcotest.fail "no <svg>");
    ]
