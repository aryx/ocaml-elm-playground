(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Brush.mli *)

type t = { radius : float; hardness : float; opacity : float }

let coverage (b : t) (d : float) : float =
  let inner = b.radius *. b.hardness in
  if d <= inner then 1.
  else if d >= b.radius then 0.
  else
    (* a smooth step from 1 at the inner radius to 0 at the edge *)
    let t = (d -. inner) /. (b.radius -. inner) in
    1. -. (t *. t *. (3. -. (2. *. t)))

let spacing (b : t) ((x0, y0) : float * float) ((x1, y1) : float * float) : (float * float) list =
  let step = Float.max 0.5 (b.radius /. 2.) in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let n = int_of_float (sqrt ((dx *. dx) +. (dy *. dy)) /. step) in
  List.init (n + 1) (fun i -> let t = if n = 0 then 0. else float_of_int i *. step /. sqrt ((dx *. dx) +. (dy *. dy)) in (x0 +. (t *. dx), y0 +. (t *. dy)))

(* the pixels a dab at (cx, cy) touches, and its coverage of each *)
let dab (b : t) (w : int) (h : int) ((cx, cy) : float * float) (f : int -> int -> float -> unit) : unit =
  let r = int_of_float (Float.ceil b.radius) in
  let ix = int_of_float cx and iy = int_of_float cy in
  for y = max 0 (iy - r - 1) to min (h - 1) (iy + r + 1) do
    for x = max 0 (ix - r - 1) to min (w - 1) (ix + r + 1) do
      let dx = float_of_int x +. 0.5 -. cx and dy = float_of_int y +. 0.5 -. cy in
      let c = coverage b (sqrt ((dx *. dx) +. (dy *. dy))) in
      if c > 0. then f x y c
    done
  done

let stroke_mask (b : t) (w : int) (h : int) (points : (float * float) list) : Mask.t =
  let m = Mask.empty w h in
  List.iter
    (fun p -> dab b w h p (fun x y c -> let v = int_of_float (Float.round (c *. 255.)) in if v > Mask.get m x y then Bytes.set m.alpha ((y * w) + x) (Char.chr v)))
    points;
  m

(* the stroke's mask at the brush's opacity, within the selection *)
let limited (b : t) (selection : Mask.t option) (m : Mask.t) : Mask.t =
  let m = { m with alpha = Bytes.map (fun c -> Char.chr (int_of_float (Float.round (float_of_int (Char.code c) *. b.opacity)))) m.alpha } in
  match selection with Some s -> Mask.intersect m s | None -> m

let paint (b : t) (colour : int * int * int) ?selection (img : Pixels.image) (points : (float * float) list) : Pixels.image =
  Composite.fill (limited b selection (stroke_mask b img.width img.height points)) colour img

let selected (selection : Mask.t option) x y = match selection with Some s -> float_of_int (Mask.get s x y) /. 255. | None -> 1.

let airbrush (b : t) ((r, g, bl) : int * int * int) ~(flow : float) ?selection (img : Pixels.image) (points : (float * float) list) : Pixels.image =
  let out = Pixels.copy img in
  List.iter
    (fun p ->
      dab b img.width img.height p (fun x y c ->
          let a = c *. flow *. b.opacity *. selected selection x y in
          List.iteri
            (fun ch v -> let old = float_of_int (Pixels.get out x y ch) in Pixels.set out x y ch (Pixels.clamp (int_of_float (Float.round (old +. ((float_of_int v -. old) *. a))))))
            [ r; g; bl ]))
    points;
  out

let stamp (b : t) ~(source : Pixels.image) ~(offset : int * int) ?selection (img : Pixels.image) (points : (float * float) list) : Pixels.image =
  let dx, dy = offset in
  let shifted = Pixels.copy img in
  for y = 0 to img.height - 1 do
    for x = 0 to img.width - 1 do
      for c = 0 to 2 do
        Pixels.set shifted x y c (Pixels.get source (x + dx) (y + dy) c)
      done
    done
  done;
  Composite.through (limited b selection (stroke_mask b img.width img.height points)) ~before:img ~after:shifted

let smudge (b : t) ~(strength : float) ?selection (img : Pixels.image) (points : (float * float) list) : Pixels.image =
  let out = Pixels.copy img in
  let rec go = function
    | (px, py) :: ((cx, cy) :: _ as rest) ->
        (* this dab takes the colours under the one before: all of them
           read first, then written, the dab's pixels only *)
        let ox = int_of_float (Float.round (px -. cx)) and oy = int_of_float (Float.round (py -. cy)) in
        let changes = ref [] in
        dab b img.width img.height (cx, cy) (fun x y c ->
            let a = c *. strength *. b.opacity *. selected selection x y in
            for ch = 0 to 2 do
              let old = float_of_int (Pixels.get out x y ch) and carried = float_of_int (Pixels.get out (x + ox) (y + oy) ch) in
              changes := (x, y, ch, Pixels.clamp (int_of_float (Float.round (old +. ((carried -. old) *. a))))) :: !changes
            done);
        List.iter (fun (x, y, ch, v) -> Pixels.set out x y ch v) !changes;
        go rest
    | _ -> ()
  in
  go points;
  out
