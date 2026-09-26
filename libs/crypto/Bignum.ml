(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Bignum.mli *)

let base_bits = 26
let mask = (1 lsl base_bits) - 1

(* the limbs, least significant first, no zero limb on top *)
type t = int array

let normalize (a : int array) : t =
  let n = ref (Array.length a) in
  while !n > 0 && a.(!n - 1) = 0 do decr n done;
  if !n = Array.length a then a else Array.sub a 0 !n

let zero : t = [||]
let one : t = [| 1 |]
let is_zero (a : t) : bool = Array.length a = 0

let of_int (n : int) : t =
  let rec go n acc = if n = 0 then acc else go (n lsr base_bits) (acc @ [ n land mask ]) in
  Array.of_list (go n [])

let compare (a : t) (b : t) : int =
  let la = Array.length a and lb = Array.length b in
  if la <> lb then compare la lb
  else
    let rec go i = if i < 0 then 0 else if a.(i) <> b.(i) then compare a.(i) b.(i) else go (i - 1) in
    go (la - 1)

let equal a b = compare a b = 0
let limb (a : t) (i : int) : int = if i < Array.length a then a.(i) else 0

let add (a : t) (b : t) : t =
  let n = max (Array.length a) (Array.length b) + 1 in
  let r = Array.make n 0 and c = ref 0 in
  for i = 0 to n - 1 do
    let x = limb a i + limb b i + !c in
    r.(i) <- x land mask;
    c := x lsr base_bits
  done;
  normalize r

let sub (a : t) (b : t) : t =
  let n = Array.length a in
  let r = Array.make n 0 and borrow = ref 0 in
  for i = 0 to n - 1 do
    let x = a.(i) - limb b i - !borrow in
    if x < 0 then (r.(i) <- x + (1 lsl base_bits); borrow := 1) else (r.(i) <- x; borrow := 0)
  done;
  if !borrow <> 0 then invalid_arg "Bignum.sub: negative";
  normalize r

let mul (a : t) (b : t) : t =
  let la = Array.length a and lb = Array.length b in
  if la = 0 || lb = 0 then zero
  else
    let r = Array.make (la + lb) 0 in
    for i = 0 to la - 1 do
      let c = ref 0 in
      for j = 0 to lb - 1 do
        let x = r.(i + j) + (a.(i) * b.(j)) + !c in
        r.(i + j) <- x land mask;
        c := x lsr base_bits
      done;
      r.(i + lb) <- r.(i + lb) + !c
    done;
    normalize r

let bits (a : t) : int =
  let n = Array.length a in
  if n = 0 then 0
  else
    let top = a.(n - 1) in
    let rec width x k = if x = 0 then k else width (x lsr 1) (k + 1) in
    ((n - 1) * base_bits) + width top 0

let bit (a : t) (i : int) : bool = (limb a (i / base_bits) lsr (i mod base_bits)) land 1 = 1

let shift_left (a : t) (k : int) : t =
  if is_zero a then a
  else
    let limbs = k / base_bits and b = k mod base_bits in
    let r = Array.make (Array.length a + limbs + 1) 0 in
    Array.iteri
      (fun i x ->
        let v = x lsl b in
        r.(i + limbs) <- r.(i + limbs) lor (v land mask);
        r.(i + limbs + 1) <- v lsr base_bits)
      a;
    normalize r

let shift_right (a : t) (k : int) : t =
  let limbs = k / base_bits and b = k mod base_bits in
  let n = Array.length a - limbs in
  if n <= 0 then zero
  else normalize (Array.init n (fun i -> (a.(i + limbs) lsr b) lor ((limb a (i + limbs + 1) lsl (base_bits - b)) land mask)))

(* a mod m, the bits of a brought down one by one, as by hand *)
let rem (a : t) (m : t) : t =
  if compare a m < 0 then a
  else
    let r = ref zero in
    for i = bits a - 1 downto 0 do
      r := shift_left !r 1;
      if bit a i then r := add !r one;
      if compare !r m >= 0 then r := sub !r m
    done;
    !r

let of_bytes (s : string) : t = String.fold_left (fun acc c -> add (shift_left acc 8) (of_int (Char.code c))) zero s

let to_bytes ~(len : int) (a : t) : string =
  String.init len (fun i ->
      let k = 8 * (len - 1 - i) in
      let v = ref 0 in
      for j = 0 to 7 do
        if bit a (k + j) then v := !v lor (1 lsl j)
      done;
      Char.chr !v)

let of_hex (s : string) : t =
  String.fold_left (fun acc c -> if c = ' ' || c = '\n' then acc else add (shift_left acc 4) (of_int (int_of_string ("0x" ^ String.make 1 c)))) zero s

let to_hex (a : t) : string =
  if is_zero a then "0"
  else
    let len = (bits a + 7) / 8 in
    let s = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq (to_bytes ~len a)))) in
    if s.[0] = '0' then String.sub s 1 (String.length s - 1) else s

(*****************************************************************************)
(* Montgomery *)
(*****************************************************************************)

(* [mp]: m padded to n limbs once, as every product wants it *)
type modulus = { m : t; mp : int array; n : int; minv : int; r2 : int array; one_m : int array }
type mont = int array (* exactly n limbs, below m *)

let pad (n : int) (a : t) : int array = Array.init n (fun i -> limb a i)

(* CIOS: a*b/R mod m, both of n limbs *)
let redc_mul (md : modulus) (a : int array) (b : int array) : int array =
  (* claude: was [m = pad md.n md.m], a new array at each of the
     thousands of products of a signature's check *)
  let n = md.n and m = md.mp in
  let t = Array.make (n + 2) 0 in
  for i = 0 to n - 1 do
    let c = ref 0 in
    let ai = a.(i) in
    for j = 0 to n - 1 do
      let x = t.(j) + (ai * b.(j)) + !c in
      t.(j) <- x land mask;
      c := x lsr base_bits
    done;
    let x = t.(n) + !c in
    t.(n) <- x land mask;
    t.(n + 1) <- t.(n + 1) + (x lsr base_bits);
    (* a multiple of m that zeroes the lowest limb, then shift by one *)
    let u = (t.(0) * md.minv) land mask in
    let x = t.(0) + (u * m.(0)) in
    let c = ref (x lsr base_bits) in
    for j = 1 to n - 1 do
      let x = t.(j) + (u * m.(j)) + !c in
      t.(j - 1) <- x land mask;
      c := x lsr base_bits
    done;
    let x = t.(n) + !c in
    t.(n - 1) <- x land mask;
    t.(n) <- t.(n + 1) + (x lsr base_bits);
    t.(n + 1) <- 0
  done;
  let r = normalize (Array.sub t 0 (n + 1)) in
  let r = if compare r md.m >= 0 then sub r md.m else r in
  pad n r

let modulus (m : t) : modulus =
  if Array.length m = 0 || m.(0) land 1 = 0 then invalid_arg "Bignum.modulus: even";
  let n = Array.length m in
  (* 1/m0 mod 2^26 by Newton's iteration, each step doubling the bits right *)
  let inv = ref 1 in
  for _ = 1 to 5 do
    inv := (!inv * (2 - (m.(0) * !inv))) land mask
  done;
  let minv = (-(!inv)) land mask in
  let r = shift_left one (base_bits * n) in
  let r2 = pad n (rem (mul r r) m) in
  let one_m = pad n (rem r m) in
  { m; mp = pad n m; n; minv; r2; one_m }

let modulus_value (md : modulus) : t = md.m
let of_nat (md : modulus) (a : t) : mont = redc_mul md (pad md.n (rem a md.m)) md.r2
let to_nat (md : modulus) (a : mont) : t = normalize (redc_mul md a (pad md.n one))
let mont_mul = redc_mul
let mont_one (md : modulus) : mont = md.one_m
let mont_is_zero (a : mont) : bool = Array.for_all (( = ) 0) a
let mont_equal (a : mont) (b : mont) : bool = a = b

(* a+b and a-b mod m, both below m, on the n limbs as they are -- no
   normalizing, one array each; the curves' doublings do a dozen a
   point. (claude: was add/sub on normalized copies, then pad: three
   arrays and a compare each) *)
let mont_add (md : modulus) (a : mont) (b : mont) : mont =
  let n = md.n in
  let r = Array.make n 0 and c = ref 0 in
  for i = 0 to n - 1 do
    let x = a.(i) + b.(i) + !c in
    r.(i) <- x land mask;
    c := x lsr base_bits
  done;
  (* above m (a carry out, or r >= m): once less m *)
  let ge =
    !c > 0
    ||
    let rec go i = if i < 0 then true else if r.(i) <> md.mp.(i) then r.(i) > md.mp.(i) else go (i - 1) in
    go (n - 1)
  in
  if ge then (
    let borrow = ref 0 in
    for i = 0 to n - 1 do
      let x = r.(i) - md.mp.(i) - !borrow in
      if x < 0 then (r.(i) <- x + (1 lsl base_bits); borrow := 1) else (r.(i) <- x; borrow := 0)
    done);
  r

let mont_sub (md : modulus) (a : mont) (b : mont) : mont =
  let n = md.n in
  let r = Array.make n 0 and borrow = ref 0 in
  for i = 0 to n - 1 do
    let x = a.(i) - b.(i) - !borrow in
    if x < 0 then (r.(i) <- x + (1 lsl base_bits); borrow := 1) else (r.(i) <- x; borrow := 0)
  done;
  (* below zero: m added back *)
  if !borrow = 1 then (
    let c = ref 0 in
    for i = 0 to n - 1 do
      let x = r.(i) + md.mp.(i) + !c in
      r.(i) <- x land mask;
      c := x lsr base_bits
    done);
  r

let pow_mod (md : modulus) (base : t) (exp : t) : t =
  let b = of_nat md base in
  let r = ref (mont_one md) in
  for i = bits exp - 1 downto 0 do
    r := redc_mul md !r !r;
    if bit exp i then r := redc_mul md !r b
  done;
  to_nat md !r

let inverse_prime (md : modulus) (a : t) : t = pow_mod md a (sub md.m (of_int 2))
let mul_mod (md : modulus) (a : t) (b : t) : t = to_nat md (redc_mul md (of_nat md a) (of_nat md b))
let add_mod (md : modulus) (a : t) (b : t) : t = let s = add a b in if compare s md.m >= 0 then sub s md.m else s
let sub_mod (md : modulus) (a : t) (b : t) : t = if compare a b >= 0 then sub a b else sub (add a md.m) b
