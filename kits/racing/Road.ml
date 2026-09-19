(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Road.mli *)

(*****************************************************************************)
(* Sections *)
(*****************************************************************************)

type section = { enter : int; hold : int; leave : int; curve : number; hill : number }

let section n curve hill = { enter = n / 4; hold = n / 2; leave = n / 4; curve; hill }
let straight n = section n 0. 0.
let curve n c = section n c 0.
let hill n h = section n 0. h
let curve_hill n c h = section n c h

let coast =
  [ straight 40; curve 80 2.; hill 80 20.; curve 80 (-4.); curve_hill 100 3. 40.; straight 40;
    curve_hill 80 (-2.) (-40.); curve 60 5.; curve 60 (-5.); hill 100 30.; curve_hill 80 4. (-30.); straight 60;
    curve 100 (-3.); hill 60 (-20.); straight 40 ]

(*****************************************************************************)
(* The track *)
(*****************************************************************************)

type segment = { index : int; y1 : number; y2 : number; curve : number }
type t = { segment_length : number; segments : segment array }

let ease_in a b p = a +. ((b -. a) *. p *. p)
let ease_in_out a b p = a +. ((b -. a) *. ((-.cos (p *. Float.pi) /. 2.) +. 0.5))

let build (segment_length : number) (sections : section list) : t =
  let segs = ref [] and y = ref 0. and index = ref 0 in
  List.iter
    (fun s ->
      let total = float_of_int (s.enter + s.hold + s.leave) in
      let y_start = !y and y_end = !y +. (s.hill *. segment_length) in
      (* the i-th segment of the section, and its curve: its far edge at
       * the fraction (i + 1) / total of the section's climb, so that the
       * last one ends at y_end (Jake Gordon's tutorial takes i / total,
       * never quite reaching it: heights drift from section to section) *)
      let add i curve =
        let y2 = ease_in_out y_start y_end (float_of_int (i + 1) /. total) in
        segs := { index = !index; y1 = !y; y2; curve } :: !segs;
        y := y2;
        incr index
      in
      for i = 0 to s.enter - 1 do add i (ease_in 0. s.curve (float_of_int i /. float_of_int s.enter)) done;
      for i = 0 to s.hold - 1 do add (s.enter + i) s.curve done;
      for i = 0 to s.leave - 1 do add (s.enter + s.hold + i) (ease_in_out s.curve 0. (float_of_int i /. float_of_int s.leave)) done)
    sections;
  { segment_length; segments = Array.of_list (List.rev !segs) }

let length (t : t) : number = float_of_int (Array.length t.segments) *. t.segment_length

let segment_at (t : t) (z : number) : segment =
  let n = Array.length t.segments in
  t.segments.(((int_of_float (floor (z /. t.segment_length)) mod n) + n) mod n)

(*****************************************************************************)
(* In space *)
(*****************************************************************************)

type point = { x : number; y : number; z : number; heading : number }

let centerline (degrees_per_curve : number) (t : t) : point array =
  let start = { x = 0.; y = 0.; z = 0.; heading = 0. } in
  let points = Array.make (Array.length t.segments + 1) start in
  Array.iteri
    (fun i (seg : segment) ->
      let p = points.(i) in
      let heading = p.heading +. (seg.curve *. degrees_per_curve) in
      let a = heading *. Float.pi /. 180. in
      points.(i + 1) <-
        { x = p.x +. (t.segment_length *. sin a); y = seg.y2; z = p.z -. (t.segment_length *. cos a); heading })
    t.segments;
  points
