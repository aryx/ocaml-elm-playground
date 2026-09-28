(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ai_debug.mli *)

open Playground

(* a segment, a thin rectangle turned (as Audio_debug draws its wave) *)
let segment (color : color) (thick : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 -. x1) (y2 -. y1) +. 1.) thick
  |> rotate (Float.atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let default_color = rgb 120 220 160

(*****************************************************************************)
(* A way *)
(*****************************************************************************)

let way ?(color = default_color) ?(dot = 4.) ~(at : int * int -> number * number) (path : (int * int) list) :
    shape list =
  let points = List.map at path in
  let joins =
    match points with
    | [] | [ _ ] -> []
    | first :: rest -> List.rev (snd (List.fold_left (fun (prev, acc) p -> (p, segment color 2. prev p :: acc)) (first, []) rest))
  in
  let dots = List.map (fun (x, y) -> circle color dot |> move x y) points in
  (* the end of it, open, so that a way of one step still shows where
   * it is going *)
  let last = match List.rev points with (x, y) :: _ -> [ circle color (dot *. 2.5) |> fade 0.4 |> move x y ] | [] -> [] in
  joins @ dots @ last

(*****************************************************************************)
(* A flow field *)
(*****************************************************************************)

let field ?(color = default_color) ?(arrow = 12.) ~(at : int * int -> number * number) (f : Ai.flow)
    (tiles : (int * int) list) : shape list =
  (* the furthest tile drawn sets the scale: an arrow is brightest at
   * the goal and faintest at the far end of the map *)
  let far =
    List.fold_left (fun m t -> match Ai.steps_to_go f t with Some k -> Float.max m k | None -> m) 1. tiles
  in
  List.concat_map
    (fun tile ->
      match (Ai.next_step f tile, Ai.steps_to_go f tile) with
      | (Some next, Some to_go) ->
          let (x, y) = at tile and (nx, ny) = at next in
          let angle = Float.atan2 (ny -. y) (nx -. x) in
          let tip = (x +. (arrow *. Float.cos angle), y +. (arrow *. Float.sin angle)) in
          let bright = 1. -. (0.7 *. (to_go /. far)) in
          [ segment color 2. (x, y) tip |> fade bright;
            triangle color (arrow /. 3.) |> rotate ((angle *. 180. /. Float.pi) -. 90.) |> fade bright
            |> move (fst tip) (snd tip) ]
      | _ -> [])
    tiles

(*****************************************************************************)
(* What an opponent makes of its moves *)
(*****************************************************************************)

let thoughts ?(color = rgb 150 160 190) ?(best = rgb 240 200 90) ?(size = 1.3) ?(width = 180.) ~(naming : 'move -> string)
    (moves : ('move * number) list) : shape list =
  match moves with
  | [] -> []
  | _ ->
      let sorted = List.sort (fun (_, a) (_, b) -> compare b a) moves in
      let scale_ = List.fold_left (fun m (_, v) -> Float.max m (Float.abs v)) 0.001 moves in
      let row = 22. in
      (* the line the bars grow from: without it, a move it hates and a
         move it loves are the same picture *)
      let zero = [ rectangle color 1.5 (row *. float_of_int (List.length sorted)) |> fade 0.5
                   |> move_y (-.(row *. float_of_int (List.length sorted - 1)) /. 2.) ] in
      zero
      @ List.concat
        (List.mapi
           (fun i (m, v) ->
             let y = -.(float_of_int i *. row) in
             let long = width *. Float.abs v /. scale_ in
             let c = if i = 0 then best else color in
             [ words c (naming m) |> scale size |> move (-.width -. 40.) y;
               (* left of the line for a move it dislikes, right for one
                  it likes: the sign is the whole story *)
               rectangle c (Float.max 1. long) 12. |> fade 0.8
               |> move ((if v < 0. then -.long else long) /. 2.) y;
               words c (Printf.sprintf "%.2f" v) |> scale (size *. 0.8) |> move (width +. 40.) y ])
           sorted)

(*****************************************************************************)
(* A machine *)
(*****************************************************************************)

let machine ?(color = rgb 150 160 190) ?(radius = 120.) ~(naming : 'mode -> string)
    (changes : ('mode, 'context) Ai.change list) (mind : 'mode Ai.mind) : shape list =
  let modes = Ai.modes changes in
  let n = List.length modes in
  if n = 0 then []
  else
    (* the modes around a circle, in the order the changes name them:
       nothing here knows what a mode means, so any arrangement is a
       guess, and a ring at least never hides an arrow behind a node *)
    let place i = let a = (Float.pi /. 2.) -. (2. *. Float.pi *. float_of_int i /. float_of_int n) in
      (radius *. Float.cos a, radius *. Float.sin a)
    in
    let places = List.mapi (fun i m -> (m, place i)) modes in
    let where m = try List.assoc m places with Not_found -> (0., 0.) in
    let now = Ai.doing mind in
    let arrows =
      List.concat_map
        (fun (from, why, target) ->
          if from = target then []
          else
            let (x1, y1) = where from and (x2, y2) = where target in
            (* short of the node itself, so the arrow ends at its edge,
               and a little to the left of the straight line, so that a
               change and the change back are two arrows and not one *)
            let a = Float.atan2 (y2 -. y1) (x2 -. x1) in
            let back = 26. in
            let aside = 9. in
            let (sx, sy) = (aside *. Float.cos (a +. (Float.pi /. 2.)), aside *. Float.sin (a +. (Float.pi /. 2.))) in
            let tip = (x2 -. (back *. Float.cos a) +. sx, y2 -. (back *. Float.sin a) +. sy) in
            let from_ = (x1 +. (back *. Float.cos a) +. sx, y1 +. (back *. Float.sin a) +. sy) in
            let lit = from = now in
            [ segment color 2. from_ tip |> fade (if lit then 0.9 else 0.3);
              triangle color 6. |> rotate ((a *. 180. /. Float.pi) -. 90.) |> fade (if lit then 0.9 else 0.3)
              |> move (fst tip) (snd tip) ]
            @
            if why = "" || not lit then []
            else
              [ words color why |> scale 1.1 |> fade 0.9
                |> move ((fst from_ +. fst tip) /. 2.) (((snd from_ +. snd tip) /. 2.) +. 12.) ])
        (Ai.links changes)
    in
    let nodes =
      List.map
        (fun (m, (x, y)) ->
          let lit = m = now in
          group
            [ circle (if lit then rgb 240 200 90 else color) 24. |> fade (if lit then 0.9 else 0.25);
              words (if lit then black else white) (naming m) |> scale 1.1 ]
          |> move x y)
        places
    in
    arrows @ nodes
    @ [ words (rgb 240 200 90) (Printf.sprintf "%s, %d frames" (naming now) (Ai.doing_for mind))
        |> scale 1.2 |> move_y (-.radius -. 40.) ]
