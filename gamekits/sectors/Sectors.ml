(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sectors.mli *)

type point = float * float

type sector = {
  floor : float;
  ceiling : float;
  light : float;
  floor_rgb : int * int * int;
  ceiling_rgb : int * int * int;
  wall_rgb : int * int * int;
  loops : point list list;
}

type line = { x1 : float; y1 : float; x2 : float; y2 : float; front : int; back : int option }
type level = { sectors : sector array; lines : line array; start : float * float * float; exit : int }

let rect x1 y1 x2 y2 : point list = [ (x1, y1); (x1, y2); (x2, y2); (x2, y1) ]
let hole x1 y1 x2 y2 : point list = List.rev (rect x1 y1 x2 y2)

(*****************************************************************************)
(* Finding the lines *)
(*****************************************************************************)

(* a loop's edges, each point to the next, the last back to the first *)
let edges (loop : point list) : (point * point) list =
  match loop with [] -> [] | first :: _ -> List.mapi (fun i a -> (a, if i = List.length loop - 1 then first else List.nth loop (i + 1))) loop

(* the edge a -> b cut at every point of [points] strictly between a
 * and b, in order *)
let cut (points : point list) (((ax, ay) as a), ((bx, by) as b)) : (point * point) list =
  let dx = bx -. ax and dy = by -. ay in
  let len2 = (dx *. dx) +. (dy *. dy) in
  let between (px, py) =
    let t = (((px -. ax) *. dx) +. ((py -. ay) *. dy)) /. len2 in
    let cross = ((px -. ax) *. dy) -. ((py -. ay) *. dx) in
    if Float.abs cross < 1e-6 && t > 1e-9 && t < 1. -. 1e-9 then Some (t, (px, py)) else None
  in
  let inner = List.filter_map between points |> List.sort_uniq compare |> List.map snd in
  let all = (a :: inner) @ [ b ] in
  List.combine (List.filteri (fun i _ -> i < List.length all - 1) all) (List.tl all)

let make (sectors : sector list) ~start ~exit : level =
  let points = List.concat_map (fun s -> List.concat s.loops) sectors in
  (* every sector's edges, cut: (a, b, the sector) *)
  let all = List.concat (List.mapi (fun i s -> List.concat_map (fun l -> List.concat_map (cut points) (edges l)) s.loops |> List.map (fun (a, b) -> (a, b, i))) sectors) in
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun (a, b, i) ->
      if Hashtbl.mem tbl (a, b) then failwith "Sectors.make: two sectors overlap";
      Hashtbl.add tbl (a, b) i)
    all;
  (* a line per edge, or per pair of opposite edges (kept once: from the
   * sector with the smaller index) *)
  let lines =
    List.filter_map
      (fun (((x1, y1) as a), ((x2, y2) as b), i) ->
        match Hashtbl.find_opt tbl (b, a) with
        | Some j when j < i -> None
        | back -> Some { x1; y1; x2; y2; front = i; back })
      all
  in
  { sectors = Array.of_list sectors; lines = Array.of_list lines; start; exit }

(*****************************************************************************)
(* Where things are *)
(*****************************************************************************)

let side (l : line) (x : float) (y : float) : float = ((l.x2 -. l.x1) *. (y -. l.y1)) -. ((l.y2 -. l.y1) *. (x -. l.x1))

let inside (s : sector) (x : float) (y : float) : bool =
  let crossings (((ax, ay), (bx, by)) : point * point) =
    if ay > y <> (by > y) && x < ax +. ((y -. ay) /. (by -. ay) *. (bx -. ax)) then 1 else 0
  in
  List.fold_left (fun n l -> List.fold_left (fun n e -> n + crossings e) n (edges l)) 0 s.loops mod 2 = 1

let sector_at (lv : level) (x : float) (y : float) : int =
  let rec go i = if i >= Array.length lv.sectors then 0 else if inside lv.sectors.(i) x y then i else go (i + 1) in
  go 0

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

let radius = 16.
let height = 56.
let step = 24.

(* the distance from (x, y) to the segment of [l] *)
let distance (l : line) (x : float) (y : float) : float =
  let dx = l.x2 -. l.x1 and dy = l.y2 -. l.y1 in
  let t = Float.max 0. (Float.min 1. ((((x -. l.x1) *. dx) +. ((y -. l.y1) *. dy)) /. ((dx *. dx) +. (dy *. dy)))) in
  Float.hypot (x -. (l.x1 +. (t *. dx))) (y -. (l.y1 +. (t *. dy)))

let move (lv : level) (sector_at : float -> float -> int) ((x, y) : float * float) ((dx, dy) : float * float) : float * float =
  let here = lv.sectors.(sector_at x y) in
  (* the line blocks: a wall, or the sector across it too high a step,
   * or too low a ceiling *)
  let blocks (l : line) =
    match l.back with
    | None -> true
    | Some back ->
        let there = lv.sectors.(if side l x y < 0. then back else l.front) in
        there.floor -. here.floor > step || Float.min there.ceiling here.ceiling -. Float.max there.floor here.floor < height
  in
  let free (nx, ny) = not (Array.exists (fun l -> distance l nx ny < radius && blocks l) lv.lines) in
  List.find_opt free [ (x +. dx, y +. dy); (x +. dx, y); (x, y +. dy) ] |> Option.value ~default:(x, y)

(*****************************************************************************)
(* The level *)
(*****************************************************************************)

(* The plan, from above, 32 map units a character across, 64 a line
   down (the steps up and down are 64 deep, 16 or 24 high):

 1024     +---------------+
          |               +-----------+
          |  upstairs 64  | corridor  |
          |               +-------+---+
  768     +---+-------+---+       |   |
              |       |           |   |
              | up    |           |dn |
              |       |           |   |
  512   +-----+-------+-------++--+---+---+
        |                     ||          |
        |                     ||      exit|
        |              window ++      +-+ |
        |  hall 0  +-+        ||      +-+ |
        |          +-+        ++          |
        |        pillar       ||          |
        |                     || dark -32 |
    0   +---------------------++----------+
        0                   768 800     1152
*)
let outpost : level =
  let sector ?(light = 0.8) ?(floor_rgb = (110, 100, 90)) ?(ceiling_rgb = (90, 90, 95)) ?(wall_rgb = (130, 120, 110)) floor
      ceiling loops =
    { floor; ceiling; light; floor_rgb; ceiling_rgb; wall_rgb; loops }
  in
  let wood = (140, 100, 60) and dark_wood = (110, 80, 50) in
  let stairs_up i = sector ~floor_rgb:wood ~wall_rgb:dark_wood (16. *. float_of_int (i + 1)) (176. +. (16. *. float_of_int i)) [ rect 256. (512. +. (64. *. float_of_int i)) 512. (576. +. (64. *. float_of_int i)) ] in
  let stairs_down i floor = sector ~light:(0.6 -. (0.05 *. float_of_int i)) ~wall_rgb:(100, 100, 110) floor (floor +. 128.) [ rect 896. (768. -. (64. *. float_of_int i)) 1024. (832. -. (64. *. float_of_int i)) ] in
  make
    ([ (* 0: the hall, a hole for the pillar *)
       sector ~light:0.85 0. 192. [ rect 0. 0. 768. 512.; hole 352. 224. 416. 288. ];
       (* 1: the pillar, a sector with no room between floor and ceiling *)
       sector ~wall_rgb:(90, 100, 130) 0. 0. [ rect 352. 224. 416. 288. ] ]
    (* 2-5: four steps up, 16 each *)
    @ List.init 4 stairs_up
    @ [ (* 6: upstairs *)
        sector ~light:1. ~floor_rgb:(90, 110, 90) ~ceiling_rgb:(170, 170, 180) ~wall_rgb:(100, 130, 100) 64. 256. [ rect 128. 768. 640. 1024. ];
        (* 7: the corridor *)
        sector ~light:0.7 64. 192. [ rect 640. 832. 1024. 960. ] ]
    (* 8-11: four steps down to the dark room *)
    @ List.mapi stairs_down [ 40.; 16.; -8. ]
    @ [ sector ~light:0.45 ~wall_rgb:(100, 100, 110) (-32.) 96. [ rect 896. 512. 1024. 640. ];
        (* 12: the dark room, a hole for the exit *)
        sector ~light:0.4 ~floor_rgb:(60, 110, 40) ~wall_rgb:(90, 90, 80) (-32.) 160. [ rect 800. 0. 1152. 512.; hole 1024. 200. 1088. 264. ];
        (* 13: the exit, raised a little, lit *)
        sector ~light:1. ~floor_rgb:(240, 210, 50) ~wall_rgb:(200, 60, 40) (-24.) 160. [ rect 1024. 200. 1088. 264. ];
        (* 14: the window, a sector too, between the hall and the dark room;
         * its sill under the eye, too high a step *)
        sector ~light:0.85 32. 112. [ rect 768. 192. 800. 320. ] ])
    ~start:(160., 96., 40.) ~exit:13
