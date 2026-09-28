(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Diode_ladder.mli *)

type t = { v : float array; (* v1..v4 *) mutable last_x : float (* the input before, the trapezoid's *) }

let create () : t = { v = Array.make 4 0.; last_x = 0. }

let reset (t : t) : unit =
  Array.fill t.v 0 4 0.;
  t.last_x <- 0.
let rate = float_of_int Signal.rate

(* the equations as v' = w (A v + b u): A's rows, b = (2, 0, 0, 0) *)
let a = [| [| -4.; 2.; 0.; 0. |]; [| 1.; -2.; 1.; 0. |]; [| 0.; 1.; -2.; 1. |]; [| 0.; 0.; 1.; -1. |] |]

(* the feedback enters through u = x - k v4: A's first row gains
 * -2 k in its last column *)
let with_feedback (k : float) : float array array =
  Array.mapi (fun i row -> Array.mapi (fun j aij -> if i = 0 && j = 3 then aij -. (2. *. k) else aij) row) a

(* [solve m y]: m x = y, 4 x 4, Gaussian elimination with partial
 * pivoting; m and y are overwritten *)
let solve (m : float array array) (y : float array) : float array =
  let n = 4 in
  for c = 0 to n - 1 do
    let p = ref c in
    for r = c + 1 to n - 1 do
      if Float.abs m.(r).(c) > Float.abs m.(!p).(c) then p := r
    done;
    let tmp = m.(c) in
    m.(c) <- m.(!p);
    m.(!p) <- tmp;
    let ty = y.(c) in
    y.(c) <- y.(!p);
    y.(!p) <- ty;
    for r = c + 1 to n - 1 do
      let f = m.(r).(c) /. m.(c).(c) in
      for j = c to n - 1 do
        m.(r).(j) <- m.(r).(j) -. (f *. m.(c).(j))
      done;
      y.(r) <- y.(r) -. (f *. y.(c))
    done
  done;
  let x = Array.make n 0. in
  for r = n - 1 downto 0 do
    let s = ref y.(r) in
    for j = r + 1 to n - 1 do
      s := !s -. (m.(r).(j) *. x.(j))
    done;
    x.(r) <- !s /. m.(r).(r)
  done;
  x

let process (t : t) ~(cutoff : Signal.t) ~(resonance : float) (s : Signal.t) : unit =
  let ak = with_feedback resonance in
  Array.iteri
    (fun i x ->
      let x = tanh x in
      let fc = Float.max 10. (Float.min 20000. cutoff.(i)) in
      (* prewarped: the trapezoid's h w / 2 = tan (pi fc / rate) *)
      let g = tan (Float.pi *. Float.min fc (0.45 *. rate) /. rate) in
      (* (I - g A) v' = (I + g A) v + g b (x + x_last) *)
      let m = Array.init 4 (fun r -> Array.init 4 (fun c -> (if r = c then 1. else 0.) -. (g *. ak.(r).(c)))) in
      let y =
        Array.init 4 (fun r ->
            let sum = ref t.v.(r) in
            for c = 0 to 3 do
              sum := !sum +. (g *. ak.(r).(c) *. t.v.(c))
            done;
            if r = 0 then !sum +. (g *. 2. *. (x +. t.last_x)) else !sum)
      in
      let v = solve m y in
      Array.blit v 0 t.v 0 4;
      t.last_x <- x;
      s.(i) <- v.(3))
    s
