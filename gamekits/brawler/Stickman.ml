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
open Basics (* float arithmetics *)

(* See Stickman.mli *)

type limb = number * number
type pose = { lean : number; front_arm : limb; back_arm : limb; front_leg : limb; back_leg : limb }

let stand = { lean = 0.; front_arm = (15., 30.); back_arm = (-15., 10.); front_leg = (8., 0.); back_leg = (-8., 0.) }

let lerp (a : pose) (b : pose) (t : number) : pose =
  let n x y = x + ((y - x) * t) in
  let l (x1, x2) (y1, y2) = (n x1 y1, n x2 y2) in
  { lean = n a.lean b.lean; front_arm = l a.front_arm b.front_arm; back_arm = l a.back_arm b.back_arm; front_leg = l a.front_leg b.front_leg; back_leg = l a.back_leg b.back_leg }

let at (keys : (int * pose) list) (frame : int) : pose =
  let rec go l =
    match l with
    | (f1, p1) :: ((f2, p2) :: _ as rest) -> if frame <= f1 then p1 else if frame < f2 then lerp p1 p2 (float_of_int (frame -.. f1) / float_of_int (f2 -.. f1)) else go rest
    | [ (_, p) ] -> p
    | [] -> stand
  in
  go keys

(* a limb's direction: from straight down, positive forward *)
let dir (a : number) : number * number = (sin (a * pi / 180.), -.cos (a * pi / 180.))
let plus (x, y) k (dx, dy) = (x + (k * dx), y + (k * dy))

(* the joints, for a figure [h] high facing right, its lowest foot on
 * (0, 0): hip, shoulder, head's center, and each limb's middle joint
 * and end *)
type joints = {
  hip : number * number;
  shoulder : number * number;
  head : number * number;
  limbs : ((number * number) * (number * number) * (number * number)) list; (* front arm, back arm, front leg, back leg: start, joint, end *)
}

let joints (h : number) (p : pose) : joints =
  let hip = (0., 0.) in
  let shoulder = plus hip (0.3 * h) (sin (p.lean * pi / 180.), cos (p.lean * pi / 180.)) in
  let head = plus shoulder (0.11 * h) (sin (p.lean * pi / 180.), cos (p.lean * pi / 180.)) in
  let limb start len (a1, a2) = let j = plus start len (dir a1) in (start, j, plus j len (dir a2)) in
  let limbs = [ limb shoulder (0.17 * h) p.front_arm; limb shoulder (0.17 * h) p.back_arm; limb hip (0.25 * h) p.front_leg; limb hip (0.25 * h) p.back_leg ] in
  let lowest = List.fold_left (fun m (_, (_, jy), (_, ey)) -> Float.min m (Float.min jy ey)) 0. (List.filteri (fun i _ -> i >= 2) limbs) in
  let up (x, y) = (x, y - lowest) in
  { hip = up hip; shoulder = up shoulder; head = up head; limbs = List.map (fun (a, b, c) -> (up a, up b, up c)) limbs }

let hand (h : number) (p : pose) : number * number = match (joints h p).limbs with (_, _, e) :: _ -> e | [] -> (0., 0.)

let segment (color : color) (width : number) ((x0, y0) : number * number) ((x1, y1) : number * number) : shape =
  group [ rectangle color (Float.hypot (x1 - x0) (y1 - y0)) width |> rotate (atan2 (y1 - y0) (x1 - x0) * 180. / pi) |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.); circle color (width / 2.) |> move x1 y1 ]

let draw (color : color) (back : color) (h : number) (facing : number) (p : pose) : shape =
  let j = joints h p in
  let w = 0.055 * h in
  let flip (x, y) = (facing * x, y) in
  let limb c (a, b, e) = [ segment c w (flip a) (flip b); segment c w (flip b) (flip e) ] in
  match j.limbs with
  | [ fa; ba; fl; bl ] ->
      group (limb back ba @ limb back bl @ [ segment color (w * 1.3) (flip j.hip) (flip j.shoulder) ] @ limb color fl @ [ circle color (0.09 * h) |> move (fst (flip j.head)) (snd j.head) ] @ limb color fa)
  | _ -> group []
