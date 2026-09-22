(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Matrix.mli *)

type t = { rows : int; cols : int; data : float array }

let create (rows : int) (cols : int) : t = { rows; cols; data = Array.make (rows * cols) 0. }

let init (rows : int) (cols : int) (f : int -> int -> float) : t =
  { rows; cols; data = Array.init (rows * cols) (fun i -> f (i / cols) (i mod cols)) }

let of_lists (rows : float list list) : t =
  match rows with
  | [] -> create 0 0
  | first :: _ ->
      let cols = List.length first in
      if List.exists (fun r -> List.length r <> cols) rows then invalid_arg "Matrix.of_lists: ragged rows";
      { rows = List.length rows; cols; data = Array.of_list (List.concat rows) }

let to_lists (m : t) : float list list =
  List.init m.rows (fun r -> List.init m.cols (fun c -> m.data.((r * m.cols) + c)))

let identity (n : int) : t = init n n (fun r c -> if r = c then 1. else 0.)

let random ~(seed : int) ?(spread = 1.) (rows : int) (cols : int) : t =
  let st = Random.State.make [| seed |] in
  { rows; cols; data = Array.init (rows * cols) (fun _ -> Random.State.float st (2. *. spread) -. spread) }

let vector (a : float array) : t = { rows = Array.length a; cols = 1; data = Array.copy a }
let to_vector (m : t) : float array = Array.copy m.data

let get (m : t) (r : int) (c : int) : float = m.data.((r * m.cols) + c)
let set (m : t) (r : int) (c : int) (v : float) : unit = m.data.((r * m.cols) + c) <- v
let row (m : t) (r : int) : float array = Array.sub m.data (r * m.cols) m.cols
let same_shape (a : t) (b : t) : bool = a.rows = b.rows && a.cols = b.cols

let map (f : float -> float) (m : t) : t = { m with data = Array.map f m.data }

let map2 (f : float -> float -> float) (a : t) (b : t) : t =
  if not (same_shape a b) then invalid_arg "Matrix: shapes differ";
  { a with data = Array.init (Array.length a.data) (fun i -> f a.data.(i) b.data.(i)) }

let add (a : t) (b : t) : t = map2 ( +. ) a b
let sub (a : t) (b : t) : t = map2 ( -. ) a b
let times (a : t) (b : t) : t = map2 ( *. ) a b
let scale (k : float) (m : t) : t = map (fun v -> k *. v) m
let sum (m : t) : float = Array.fold_left ( +. ) 0. m.data
let transpose (m : t) : t = init m.cols m.rows (fun r c -> get m c r)

(* the definition: c(i,j) is the ith row of a against the jth column of
 * b. Reading that column is a jump of b.cols floats per step, which is
 * what the other version is about *)
let mul_simple (a : t) (b : t) : t =
  if a.cols <> b.rows then invalid_arg "Matrix.mul: inner dimensions differ";
  let c = create a.rows b.cols in
  for i = 0 to a.rows - 1 do
    for j = 0 to b.cols - 1 do
      let s = ref 0. in
      for k = 0 to a.cols - 1 do
        s := !s +. (a.data.((i * a.cols) + k) *. b.data.((k * b.cols) + j))
      done;
      c.data.((i * c.cols) + j) <- !s
    done
  done;
  c

(* the same product, reading along rows only: b is copied transposed
 * first (one pass over it, which the whole product then pays back),
 * and the inner loop adds four at a time -- four sums that do not
 * depend on each other, so the processor can work on them at once *)
let mul_fast (a : t) (b : t) : t =
  if a.cols <> b.rows then invalid_arg "Matrix.mul: inner dimensions differ";
  let n = a.cols in
  let bt = transpose b in
  let c = create a.rows b.cols in
  for i = 0 to a.rows - 1 do
    let arow = i * n in
    for j = 0 to b.cols - 1 do
      let brow = j * n in
      let s0 = ref 0. and s1 = ref 0. and s2 = ref 0. and s3 = ref 0. in
      let k = ref 0 in
      while !k + 3 < n do
        s0 := !s0 +. (a.data.(arow + !k) *. bt.data.(brow + !k));
        s1 := !s1 +. (a.data.(arow + !k + 1) *. bt.data.(brow + !k + 1));
        s2 := !s2 +. (a.data.(arow + !k + 2) *. bt.data.(brow + !k + 2));
        s3 := !s3 +. (a.data.(arow + !k + 3) *. bt.data.(brow + !k + 3));
        k := !k + 4
      done;
      while !k < n do
        s0 := !s0 +. (a.data.(arow + !k) *. bt.data.(brow + !k));
        incr k
      done;
      c.data.((i * c.cols) + j) <- !s0 +. !s1 +. !s2 +. !s3
    done
  done;
  c

let fast = ref true
let mul (a : t) (b : t) : t = if !fast then mul_fast a b else mul_simple a b
