(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mask.mli *)

type t = { width : int; height : int; alpha : Bytes.t }

let make w h v = { width = w; height = h; alpha = Bytes.make (w * h) (Char.chr v) }
let empty w h = make w h 0
let all w h = make w h 255
let get (m : t) x y = if x < 0 || y < 0 || x >= m.width || y >= m.height then 0 else Char.code (Bytes.get m.alpha ((y * m.width) + x))
let set (m : t) x y v = Bytes.set m.alpha ((y * m.width) + x) (Char.chr v)

let init w h (f : int -> int -> int) : t =
  let m = empty w h in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      set m x y (f x y)
    done
  done;
  m

let rectangle w h (x0, y0) (x1, y1) =
  let xa = min x0 x1 and xb = max x0 x1 and ya = min y0 y1 and yb = max y0 y1 in
  init w h (fun x y -> if x >= xa && x < xb && y >= ya && y < yb then 255 else 0)

let ellipse w h (x0, y0) (x1, y1) =
  let xa = float_of_int (min x0 x1) and xb = float_of_int (max x0 x1) and ya = float_of_int (min y0 y1) and yb = float_of_int (max y0 y1) in
  let cx = (xa +. xb) /. 2. and cy = (ya +. yb) /. 2. and rx = (xb -. xa) /. 2. and ry = (yb -. ya) /. 2. in
  if rx <= 0. || ry <= 0. then empty w h
  else
    init w h (fun x y ->
        (* 16 points in the pixel: how many are inside *)
        let inside = ref 0 in
        for j = 0 to 3 do
          for i = 0 to 3 do
            let px = float_of_int x +. ((float_of_int i +. 0.5) /. 4.) and py = float_of_int y +. ((float_of_int j +. 0.5) /. 4.) in
            let dx = (px -. cx) /. rx and dy = (py -. cy) /. ry in
            if (dx *. dx) +. (dy *. dy) <= 1. then incr inside
          done
        done;
        !inside * 255 / 16)

let polygon w h (points : (float * float) list) =
  let pts = Array.of_list points in
  let n = Array.length pts in
  if n < 3 then empty w h
  else
    init w h (fun x y ->
        let px = float_of_int x +. 0.5 and py = float_of_int y +. 0.5 in
        (* even-odd: the edges a ray going right from the centre crosses *)
        let crossings = ref 0 in
        for i = 0 to n - 1 do
          let ax, ay = pts.(i) and bx, by = pts.((i + 1) mod n) in
          if (ay > py) <> (by > py) then
            let cross = ax +. ((py -. ay) /. (by -. ay) *. (bx -. ax)) in
            if px < cross then incr crossings
        done;
        if !crossings mod 2 = 1 then 255 else 0)

let wand ?(contiguous = true) ~(tolerance : int) (img : Pixels.image) (x0 : int) (y0 : int) : t =
  let w = img.width and h = img.height in
  let seed c = Pixels.get img x0 y0 c in
  let near x y = abs (Pixels.get img x y 0 - seed 0) <= tolerance && abs (Pixels.get img x y 1 - seed 1) <= tolerance && abs (Pixels.get img x y 2 - seed 2) <= tolerance in
  if not contiguous then init w h (fun x y -> if near x y then 255 else 0)
  else begin
    (* a flood fill with a stack of pixels to look at *)
    let m = empty w h in
    let stack = ref [ (x0, y0) ] in
    while !stack <> [] do
      match !stack with
      | (x, y) :: rest ->
          stack := rest;
          if x >= 0 && y >= 0 && x < w && y < h && get m x y = 0 && near x y then begin
            set m x y 255;
            stack := (x + 1, y) :: (x - 1, y) :: (x, y + 1) :: (x, y - 1) :: !stack
          end
      | [] -> ()
    done;
    m
  end

let feather ~(radius : float) (m : t) : t =
  if radius <= 0. then m
  else
    (* the mask as a grey image, blurred, back *)
    let img = Rgba_image.create ~width:m.width ~height:m.height in
    for y = 0 to m.height - 1 do
      for x = 0 to m.width - 1 do
        Pixels.set img x y 0 (get m x y)
      done
    done;
    let b = Gaussian.blur ~radius img in
    init m.width m.height (fun x y -> Pixels.get b x y 0)

let combine f (a : t) (b : t) = init a.width a.height (fun x y -> f (get a x y) (get b x y))
let invert (m : t) = init m.width m.height (fun x y -> 255 - get m x y)
let union = combine max
let intersect = combine min
let subtract = combine (fun a b -> max 0 (a - b))
let is_empty (m : t) = not (Bytes.exists (fun c -> c <> '\000') m.alpha)

let bounds (m : t) =
  let x0 = ref max_int and y0 = ref max_int and x1 = ref (-1) and y1 = ref (-1) in
  for y = 0 to m.height - 1 do
    for x = 0 to m.width - 1 do
      if get m x y > 0 then begin
        x0 := min !x0 x;
        y0 := min !y0 y;
        x1 := max !x1 (x + 1);
        y1 := max !y1 (y + 1)
      end
    done
  done;
  if !x1 < 0 then None else Some (!x0, !y0, !x1, !y1)

let edges (m : t) =
  let inside x y = get m x y >= 128 in
  let acc = ref [] in
  for y = 0 to m.height do
    for x = 0 to m.width do
      (* the pixel's top side, between (x, y - 1) and (x, y); its left
         side, between (x - 1, y) and (x, y) *)
      if x < m.width && inside x (y - 1) <> inside x y then acc := ((x, y), (x + 1, y)) :: !acc;
      if y < m.height && inside (x - 1) y <> inside x y then acc := ((x, y), (x, y + 1)) :: !acc
    done
  done;
  !acc
