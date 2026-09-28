(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Aes.mli *)

(* multiplication by x (that is 2) in GF(2^8), modulo x^8+x^4+x^3+x+1 *)
let xtime (a : int) : int = ((a lsl 1) lxor (if a land 0x80 <> 0 then 0x1b else 0)) land 0xff

let rec gmul (a : int) (b : int) : int = if b = 0 then 0 else (if b land 1 <> 0 then a else 0) lxor gmul (xtime a) (b lsr 1)

let sbox : int array =
  (* the powers of 3 go through every non-zero element: its logarithms *)
  let exp = Array.make 256 0 and log = Array.make 256 0 in
  let x = ref 1 in
  for i = 0 to 254 do
    exp.(i) <- !x;
    log.(!x) <- i;
    x := gmul !x 3
  done;
  let inverse a = if a = 0 then 0 else exp.((255 - log.(a)) mod 255) in
  let rotl8 b n = ((b lsl n) lor (b lsr (8 - n))) land 0xff in
  Array.init 256 (fun a ->
      let b = inverse a in
      b lxor rotl8 b 1 lxor rotl8 b 2 lxor rotl8 b 3 lxor rotl8 b 4 lxor 0x63)

type key = { rounds : int; w : int array (* the expanded key, a byte each *) }

let expand (k : string) : key =
  let nk = String.length k / 4 in
  let rounds = nk + 6 in
  let words = 4 * (rounds + 1) in
  let w = Array.make (4 * words) 0 in
  String.iteri (fun i c -> w.(i) <- Char.code c) k;
  let rcon = ref 1 in
  for i = nk to words - 1 do
    let t = Array.init 4 (fun j -> w.((4 * (i - 1)) + j)) in
    let t =
      if i mod nk = 0 then begin
        (* RotWord, SubWord, and the round constant *)
        let t = [| sbox.(t.(1)) lxor !rcon; sbox.(t.(2)); sbox.(t.(3)); sbox.(t.(0)) |] in
        rcon := xtime !rcon;
        t
      end
      else if nk > 6 && i mod nk = 4 then Array.map (fun b -> sbox.(b)) t
      else t
    in
    for j = 0 to 3 do
      w.((4 * i) + j) <- w.((4 * (i - nk)) + j) lxor t.(j)
    done
  done;
  { rounds; w }

let encrypt_block (k : key) (block : string) : string =
  let s = Array.init 16 (fun i -> Char.code block.[i]) in
  let add_round_key r = for i = 0 to 15 do s.(i) <- s.(i) lxor k.w.((16 * r) + i) done in
  (* the state column by column: byte i is row (i mod 4), column (i / 4) *)
  let sub_shift () =
    let t = Array.copy s in
    for c = 0 to 3 do
      for r = 0 to 3 do
        s.((4 * c) + r) <- sbox.(t.((4 * ((c + r) mod 4)) + r))
      done
    done
  in
  let mix () =
    for c = 0 to 3 do
      let a = Array.init 4 (fun r -> s.((4 * c) + r)) in
      for r = 0 to 3 do
        s.((4 * c) + r) <- xtime a.(r) lxor (xtime a.((r + 1) mod 4) lxor a.((r + 1) mod 4)) lxor a.((r + 2) mod 4) lxor a.((r + 3) mod 4)
      done
    done
  in
  add_round_key 0;
  for r = 1 to k.rounds - 1 do
    sub_shift ();
    mix ();
    add_round_key r
  done;
  sub_shift ();
  add_round_key k.rounds;
  String.init 16 (fun i -> Char.chr s.(i))
