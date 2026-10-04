(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Tensor.mli *)

(* a node: a whole matrix of values, a matrix of the same shape for
 * their slopes, what it was made from, and how to send its slopes
 * back to those. Grad's node, a matrix at a time *)
type t = {
  v : Matrix.t;
  d : Matrix.t; (* the slopes, filled in place by [backward] *)
  from : t list;
  send_back : t -> unit;
  mutable walk : int; (* the last walk that came through, see [order] *)
}

let nothing (_ : t) : unit = ()
let make (v : Matrix.t) (from : t list) (send_back : t -> unit) : t =
  { v; d = Matrix.create v.rows v.cols; from; send_back; walk = 0 }

let value (v : Matrix.t) : t = make v [] nothing
let of_ (n : t) : Matrix.t = n.v
let slope (n : t) : Matrix.t = n.d
let number (n : t) : float = n.v.data.(0)

(* slopes add up: a value used twice hears from both *)
let pour (into : Matrix.t) (m : Matrix.t) : unit =
  for i = 0 to Array.length m.data - 1 do
    into.data.(i) <- into.data.(i) +. m.data.(i)
  done

(*****************************************************************************)
(* Arithmetic *)
(*****************************************************************************)

let add (a : t) (b : t) : t =
  make (Matrix.add a.v b.v) [ a; b ] (fun n ->
      pour a.d n.d;
      pour b.d n.d)

let sub (a : t) (b : t) : t =
  make (Matrix.sub a.v b.v) [ a; b ] (fun n ->
      pour a.d n.d;
      pour b.d (Matrix.scale (-1.) n.d))

(* the matrix product. Grad's rule for a product, "each input's slope
 * is the other input", with the shapes made to fit: the slope of
 * C = A B is dC B^T for A and A^T dC for B *)
let mul (a : t) (b : t) : t =
  make (Matrix.mul a.v b.v) [ a; b ] (fun n ->
      pour a.d (Matrix.mul n.d (Matrix.transpose b.v));
      pour b.d (Matrix.mul (Matrix.transpose a.v) n.d))

(* a times b transposed, the transpose never made. With [direct] off
 * it is [mul a (transpose b)], the same numbers the long way: a copy
 * of b turned, turned back inside the product, and two more products
 * with two more copies on the way back. Directly, the slopes go back
 * a row at a time: for each number g of the output's slope, at row i
 * and column j, row i of a gets g times row j of b, and row j of b
 * gets g times row i of a -- "each input's slope is the other input",
 * once more *)
let direct = ref true

let mul_t (a : t) (b : t) : t =
  if not !direct then mul a (make (Matrix.transpose b.v) [ b ] (fun n -> pour b.d (Matrix.transpose n.d)))
  else
    make (Matrix.mul_t a.v b.v) [ a; b ] (fun n ->
        let wide = a.v.cols and outs = b.v.rows in
        for i = 0 to a.v.rows - 1 do
          for j = 0 to outs - 1 do
            let g = n.d.data.((i * outs) + j) in
            if g <> 0. then (
              Matrix.add_scaled a.d.data (i * wide) g b.v.data (j * wide) wide;
              Matrix.add_scaled b.d.data (j * wide) g a.v.data (i * wide) wide)
          done
        done)

(* element by element *)
let times (a : t) (b : t) : t =
  make (Matrix.times a.v b.v) [ a; b ] (fun n ->
      pour a.d (Matrix.times n.d b.v);
      pour b.d (Matrix.times n.d a.v))

let scale (k : float) (a : t) : t = make (Matrix.scale k a.v) [ a ] (fun n -> pour a.d (Matrix.scale k n.d))
let shift (k : float) (a : t) : t = make (Matrix.map (fun x -> x +. k) a.v) [ a ] (fun n -> pour a.d n.d)

let transpose (a : t) : t = make (Matrix.transpose a.v) [ a ] (fun n -> pour a.d (Matrix.transpose n.d))

(* one function of each number, given with its derivative as a
 * function of the input and the output *)
let each (f : float -> float) (df : float -> float -> float) (a : t) : t =
  make (Matrix.map f a.v) [ a ] (fun n ->
      for i = 0 to Array.length a.v.data - 1 do
        a.d.data.(i) <- a.d.data.(i) +. (n.d.data.(i) *. df a.v.data.(i) n.v.data.(i))
      done)

let tanh_ (a : t) : t = each tanh (fun _ y -> 1. -. (y *. y)) a
let relu (a : t) : t = each (fun x -> if x > 0. then x else 0.) (fun x _ -> if x > 0. then 1. else 0.) a
let pow (a : t) (k : float) : t = each (fun x -> x ** k) (fun x _ -> k *. (x ** (k -. 1.))) a

(*****************************************************************************)
(* Picking and joining *)
(*****************************************************************************)

(* the rows asked for, in the order asked: looking tokens up in a
 * table. A row asked for twice hears from both *)
let rows (a : t) (which : int array) : t =
  let cols = a.v.cols in
  make
    (Matrix.init (Array.length which) cols (fun r c -> Matrix.get a.v which.(r) c))
    [ a ]
    (fun n ->
      Array.iteri
        (fun r from ->
          for c = 0 to cols - 1 do
            a.d.data.((from * cols) + c) <- a.d.data.((from * cols) + c) +. n.d.data.((r * cols) + c)
          done)
        which)

(* [count] columns from [first] *)
let cols (a : t) (first : int) (count : int) : t =
  make
    (Matrix.init a.v.rows count (fun r c -> Matrix.get a.v r (first + c)))
    [ a ]
    (fun n ->
      for r = 0 to a.v.rows - 1 do
        for c = 0 to count - 1 do
          let i = (r * a.v.cols) + first + c in
          a.d.data.(i) <- a.d.data.(i) +. n.d.data.((r * count) + c)
        done
      done)

(* side by side *)
let join_cols (parts : t list) : t =
  let rows = (List.hd parts).v.rows in
  let wide = List.fold_left (fun w p -> w + p.v.cols) 0 parts in
  let v = Matrix.create rows wide in
  let at = ref 0 in
  List.iter
    (fun p ->
      for r = 0 to rows - 1 do
        for c = 0 to p.v.cols - 1 do
          v.data.((r * wide) + !at + c) <- p.v.data.((r * p.v.cols) + c)
        done
      done;
      at := !at + p.v.cols)
    parts;
  make v parts (fun n ->
      let at = ref 0 in
      List.iter
        (fun p ->
          for r = 0 to rows - 1 do
            for c = 0 to p.v.cols - 1 do
              let i = (r * p.v.cols) + c in
              p.d.data.(i) <- p.d.data.(i) +. n.d.data.((r * wide) + !at + c)
            done
          done;
          at := !at + p.v.cols)
        parts)

(*****************************************************************************)
(* A row at a time *)
(*****************************************************************************)

(* each row's mean: a column *)
let row_mean (a : t) : t =
  let cols = a.v.cols in
  make
    (Matrix.init a.v.rows 1 (fun r _ -> Array.fold_left ( +. ) 0. (Matrix.row a.v r) /. float_of_int cols))
    [ a ]
    (fun n ->
      for r = 0 to a.v.rows - 1 do
        for c = 0 to cols - 1 do
          a.d.data.((r * cols) + c) <- a.d.data.((r * cols) + c) +. (n.d.data.(r) /. float_of_int cols)
        done
      done)

(* each row of [a] times its number in the column [s] *)
let scale_rows (a : t) (s : t) : t =
  let cols = a.v.cols in
  make
    (Matrix.init a.v.rows cols (fun r c -> Matrix.get a.v r c *. s.v.data.(r)))
    [ a; s ]
    (fun n ->
      for r = 0 to a.v.rows - 1 do
        for c = 0 to cols - 1 do
          let i = (r * cols) + c in
          a.d.data.(i) <- a.d.data.(i) +. (n.d.data.(i) *. s.v.data.(r));
          s.d.data.(r) <- s.d.data.(r) +. (n.d.data.(i) *. a.v.data.(i))
        done
      done)

(* a row's numbers, up to [upto], as shares summing to 1, the rest 0 *)
let shares (scores : Matrix.t) (r : int) (upto : int) : float array =
  let top = ref neg_infinity in
  for c = 0 to upto - 1 do
    top := Float.max !top (Matrix.get scores r c)
  done;
  let es = Array.init upto (fun c -> exp (Matrix.get scores r c -. !top)) in
  let total = Array.fold_left ( +. ) 0. es in
  Array.map (fun e -> e /. total) es

(* softmax along each row; [causal]: row r over its first r + 1
 * numbers only, the others 0 -- a token sees itself and those before.
 * The slope of a softmax: p_j (g_j - sum_k g_k p_k) *)
let softmax_rows ?(causal = false) (a : t) : t =
  let cols = a.v.cols in
  let upto r = if causal then min cols (r + 1) else cols in
  let v = Matrix.create a.v.rows cols in
  for r = 0 to a.v.rows - 1 do
    Array.iteri (fun c p -> v.data.((r * cols) + c) <- p) (shares a.v r (upto r))
  done;
  make v [ a ] (fun n ->
      for r = 0 to a.v.rows - 1 do
        let weighed = ref 0. in
        for c = 0 to upto r - 1 do
          weighed := !weighed +. (n.d.data.((r * cols) + c) *. n.v.data.((r * cols) + c))
        done;
        for c = 0 to upto r - 1 do
          let i = (r * cols) + c in
          a.d.data.(i) <- a.d.data.(i) +. (n.v.data.(i) *. (n.d.data.(i) -. !weighed))
        done
      done)

(* the mean over the rows of -log (the softmax of the row).(answer):
 * softmax and the loss as one operation, because their slope together
 * is the simple one -- the share given minus the share deserved *)
let cross_entropy (scores : t) (answers : int array) : t =
  let rows = scores.v.rows and cols = scores.v.cols in
  let p = Array.init rows (fun r -> shares scores.v r cols) in
  let loss = ref 0. in
  Array.iteri (fun r answer -> loss := !loss -. log p.(r).(answer)) answers;
  make
    (Matrix.of_lists [ [ !loss /. float_of_int rows ] ])
    [ scores ]
    (fun n ->
      let g = n.d.data.(0) /. float_of_int rows in
      for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
          let deserved = if c = answers.(r) then 1. else 0. in
          scores.d.data.((r * cols) + c) <- scores.d.data.((r * cols) + c) +. (g *. (p.(r).(c) -. deserved))
        done
      done)

(* every number added: a matrix of one *)
let sum (a : t) : t =
  make (Matrix.of_lists [ [ Matrix.sum a.v ] ]) [ a ] (fun n ->
      for i = 0 to Array.length a.d.data - 1 do
        a.d.data.(i) <- a.d.data.(i) +. n.d.data.(0)
      done)

(*****************************************************************************)
(* Going backwards *)
(*****************************************************************************)

(* Grad's walk, unchanged: the nodes deepest first, each walk its
 * number, the way back in a list of its own *)
let walks = ref 0

let order (root : t) : t list =
  incr walks;
  let walk = !walks in
  let out = ref [] in
  let rec go (path : (t * t list) list) : unit =
    match path with
    | [] -> ()
    | (n, []) :: back ->
        out := n :: !out;
        go back
    | (n, input :: rest) :: back ->
        if input.walk = walk then go ((n, rest) :: back)
        else (
          input.walk <- walk;
          go ((input, input.from) :: (n, rest) :: back))
  in
  root.walk <- walk;
  go [ (root, root.from) ];
  !out

let backward (root : t) : unit =
  let nodes = order root in
  List.iter (fun n -> Array.fill n.d.data 0 (Array.length n.d.data) 0.) nodes;
  Array.fill root.d.data 0 (Array.length root.d.data) 1.;
  List.iter (fun n -> n.send_back n) nodes

let nodes (root : t) : int = List.length (order root)
