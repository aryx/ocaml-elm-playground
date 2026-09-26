(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Rsa.mli *)

type hash = Sha256 | Sha384 | Sha512

let digest = function Sha256 -> Sha256.digest | Sha384 -> Sha512.digest384 | Sha512 -> Sha512.digest

(* the DER of DigestInfo less the hash's bytes (RFC 8017, 9.2, note 1) *)
let digest_info = function
  | Sha256 -> "\x30\x31\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x01\x05\x00\x04\x20"
  | Sha384 -> "\x30\x41\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x02\x05\x00\x04\x30"
  | Sha512 -> "\x30\x51\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x03\x05\x00\x04\x40"

(* s^e mod n, as many bytes as n *)
let apply ~(n : Bignum.t) ~(e : Bignum.t) (signature : string) : string option =
  let k = (Bignum.bits n + 7) / 8 in
  let s = Bignum.of_bytes signature in
  if String.length signature <> k || Bignum.compare s n >= 0 || Bignum.bits n < 1024 then None
  else Some (Bignum.to_bytes ~len:k (Bignum.pow_mod (Bignum.modulus n) s e))

let verify_pkcs1 ~n ~e (h : hash) ~(message : string) ~(signature : string) : bool =
  match apply ~n ~e signature with
  | None -> false
  | Some em ->
      let t = digest_info h ^ digest h message in
      let k = String.length em in
      k >= String.length t + 11 && em = "\x00\x01" ^ String.make (k - String.length t - 3) '\xff' ^ "\x00" ^ t

let mgf1 (h : hash) (seed : string) (len : int) : string =
  let out = Buffer.create len in
  let rec go i =
    if Buffer.length out < len then (
      Buffer.add_string out (digest h (seed ^ String.init 4 (fun k -> Char.chr ((i lsr (24 - (8 * k))) land 0xff))));
      go (i + 1))
  in
  go 0;
  Buffer.sub out 0 len

let verify_pss ~n ~e (h : hash) ~(message : string) ~(signature : string) : bool =
  match apply ~n ~e signature with
  | None -> false
  | Some em ->
      let mod_bits = Bignum.bits n in
      let em_bits = mod_bits - 1 in
      let em_len = (em_bits + 7) / 8 in
      (* a modulus of 8k+1 bits: the first byte of k+1 is a zero to drop *)
      let em = if String.length em > em_len then (if em.[0] = '\000' then String.sub em 1 em_len else "") else em in
      let h_len = String.length (digest h "") in
      String.length em = em_len
      && em_len >= (2 * h_len) + 2
      && em.[em_len - 1] = '\xbc'
      &&
      let masked = String.sub em 0 (em_len - h_len - 1) and hh = String.sub em (em_len - h_len - 1) h_len in
      let top = (8 * em_len) - em_bits in
      Char.code masked.[0] lsr (8 - top) = 0
      &&
      let mask = mgf1 h hh (String.length masked) in
      let db = String.mapi (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i])) masked in
      let db = String.mapi (fun i c -> if i = 0 then Char.chr (Char.code c land (0xff lsr top)) else c) db in
      let ps_len = em_len - (2 * h_len) - 2 in
      String.sub db 0 ps_len = String.make ps_len '\000'
      && db.[ps_len] = '\001'
      &&
      let salt = String.sub db (ps_len + 1) h_len in
      digest h (String.make 8 '\000' ^ digest h message ^ salt) = hh
