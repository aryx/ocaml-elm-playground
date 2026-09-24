(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Fm_algorithm.mli *)

type t = { number : int; edges : (int * int) list; carriers : int list; feedback : int * int }

(*****************************************************************************)
(* The 32 *)
(*****************************************************************************)

let all : t list =
  [
    { number = 1; edges = [ (6, 5); (5, 4); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (6, 6) };
    { number = 2; edges = [ (6, 5); (5, 4); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (2, 2) };
    { number = 3; edges = [ (6, 5); (5, 4); (3, 2); (2, 1) ]; carriers = [ 4; 1 ]; feedback = (6, 6) };
    { number = 4; edges = [ (6, 5); (5, 4); (3, 2); (2, 1) ]; carriers = [ 4; 1 ]; feedback = (4, 6) };
    { number = 5; edges = [ (6, 5); (4, 3); (2, 1) ]; carriers = [ 5; 3; 1 ]; feedback = (6, 6) };
    { number = 6; edges = [ (6, 5); (4, 3); (2, 1) ]; carriers = [ 5; 3; 1 ]; feedback = (5, 6) };
    { number = 7; edges = [ (6, 5); (5, 3); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (6, 6) };
    { number = 8; edges = [ (6, 5); (5, 3); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (4, 4) };
    { number = 9; edges = [ (6, 5); (5, 3); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (2, 2) };
    { number = 10; edges = [ (6, 4); (5, 4); (3, 2); (2, 1) ]; carriers = [ 4; 1 ]; feedback = (3, 3) };
    { number = 11; edges = [ (6, 4); (5, 4); (3, 2); (2, 1) ]; carriers = [ 4; 1 ]; feedback = (6, 6) };
    { number = 12; edges = [ (6, 3); (5, 3); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (2, 2) };
    { number = 13; edges = [ (6, 3); (5, 3); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (6, 6) };
    { number = 14; edges = [ (6, 4); (5, 4); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (6, 6) };
    { number = 15; edges = [ (6, 4); (5, 4); (4, 3); (2, 1) ]; carriers = [ 3; 1 ]; feedback = (2, 2) };
    { number = 16; edges = [ (6, 5); (4, 3); (5, 1); (3, 1); (2, 1) ]; carriers = [ 1 ]; feedback = (6, 6) };
    { number = 17; edges = [ (6, 5); (4, 3); (5, 1); (3, 1); (2, 1) ]; carriers = [ 1 ]; feedback = (2, 2) };
    { number = 18; edges = [ (6, 5); (5, 4); (4, 1); (3, 1); (2, 1) ]; carriers = [ 1 ]; feedback = (3, 3) };
    { number = 19; edges = [ (6, 5); (6, 4); (3, 2); (2, 1) ]; carriers = [ 5; 4; 1 ]; feedback = (6, 6) };
    { number = 20; edges = [ (6, 4); (5, 4); (3, 2); (3, 1) ]; carriers = [ 4; 2; 1 ]; feedback = (3, 3) };
    { number = 21; edges = [ (6, 5); (6, 4); (3, 2); (3, 1) ]; carriers = [ 5; 4; 2; 1 ]; feedback = (3, 3) };
    { number = 22; edges = [ (6, 5); (6, 4); (6, 3); (2, 1) ]; carriers = [ 5; 4; 3; 1 ]; feedback = (6, 6) };
    { number = 23; edges = [ (6, 5); (6, 4); (3, 2) ]; carriers = [ 5; 4; 2; 1 ]; feedback = (6, 6) };
    { number = 24; edges = [ (6, 5); (6, 4); (6, 3) ]; carriers = [ 5; 4; 3; 2; 1 ]; feedback = (6, 6) };
    { number = 25; edges = [ (6, 5); (6, 4) ]; carriers = [ 5; 4; 3; 2; 1 ]; feedback = (6, 6) };
    { number = 26; edges = [ (6, 4); (5, 4); (3, 2) ]; carriers = [ 4; 2; 1 ]; feedback = (6, 6) };
    { number = 27; edges = [ (6, 4); (5, 4); (3, 2) ]; carriers = [ 4; 2; 1 ]; feedback = (3, 3) };
    { number = 28; edges = [ (5, 4); (4, 3); (2, 1) ]; carriers = [ 6; 3; 1 ]; feedback = (5, 5) };
    { number = 29; edges = [ (6, 5); (4, 3) ]; carriers = [ 5; 3; 2; 1 ]; feedback = (6, 6) };
    { number = 30; edges = [ (5, 4); (4, 3) ]; carriers = [ 6; 3; 2; 1 ]; feedback = (5, 5) };
    { number = 31; edges = [ (6, 5) ]; carriers = [ 5; 4; 3; 2; 1 ]; feedback = (6, 6) };
    { number = 32; edges = []; carriers = [ 6; 5; 4; 3; 2; 1 ]; feedback = (6, 6) };
  ]

let table = Array.of_list all
let get (n : int) : t = table.(max 1 (min 32 n) - 1)
let modulators (alg : t) (op : int) : int list = List.filter_map (fun (m, o) -> if o = op then Some m else None) alg.edges

(*****************************************************************************)
(* Running them *)
(*****************************************************************************)

type state = {
  phases : float array;
  outputs : float array; (* this sample's *)
  previous : float array; (* the one before *)
  mutable earlier : float array; (* and before it: the feedback's average *)
}

let create () : state = { phases = Array.make 6 0.; outputs = Array.make 6 0.; previous = Array.make 6 0.; earlier = Array.make 6 0. }

let sample (alg : t) (s : state) ~(feedback : int) ~(increments : float array) ~(amplitudes : float array) : float =
  Array.blit s.previous 0 s.earlier 0 6;
  Array.blit s.outputs 0 s.previous 0 6;
  let from, into = alg.feedback in
  for op = 6 downto 1 do
    let i = op - 1 in
    let modulation = List.fold_left (fun acc m -> acc +. s.outputs.(m - 1)) 0. (modulators alg op) in
    (* the fed-back operator's last two outputs, averaged: from a lower
     * operator (algorithms 4 and 6) they are the previous samples' *)
    let fb =
      if op = into && feedback > 0 then
        (s.previous.(from - 1) +. s.earlier.(from - 1)) /. 2. *. Float.pow 2. (float_of_int (feedback - 8))
      else 0.
    in
    s.outputs.(i) <- amplitudes.(i) *. sin (2. *. Float.pi *. (s.phases.(i) +. modulation +. fb));
    s.phases.(i) <- s.phases.(i) +. increments.(i);
    if s.phases.(i) >= 1. then s.phases.(i) <- s.phases.(i) -. Float.of_int (Float.to_int s.phases.(i))
  done;
  List.fold_left (fun acc c -> acc +. s.outputs.(c - 1)) 0. alg.carriers

let output (s : state) (op : int) : float = s.outputs.(op - 1)
