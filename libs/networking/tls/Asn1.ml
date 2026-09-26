(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Asn1.mli *)

type t = { tag : int; value : string; raw : string }

let parse (s : string) (pos : int) : (t * int, string) result =
  let n = String.length s in
  if pos + 2 > n then Error "DER: truncated"
  else
    let tag = Char.code s.[pos] in
    if tag land 0x1f = 0x1f then Error "DER: long tags are not used in certificates"
    else
      let l0 = Char.code s.[pos + 1] in
      let len, start =
        if l0 < 0x80 then (Some l0, pos + 2)
        else
          let k = l0 land 0x7f in
          if k = 0 || k > 4 || pos + 2 + k > n then (None, 0)
          else
            let v = ref 0 in
            for i = 0 to k - 1 do
              v := (!v lsl 8) lor Char.code s.[pos + 2 + i]
            done;
            (Some !v, pos + 2 + k)
      in
      match len with
      | Some len when start + len <= n -> Ok ({ tag; value = String.sub s start len; raw = String.sub s pos (start + len - pos) }, start + len)
      | _ -> Error "DER: bad length"

let children (v : t) : t list =
  if v.tag land 0x20 = 0 then []
  else
    let rec go pos acc = if pos >= String.length v.value then List.rev acc else match parse v.value pos with Ok (c, next) -> go next (c :: acc) | Error _ -> List.rev acc in
    go 0 []

let oid (v : t) : string =
  let s = v.value in
  if s = "" then ""
  else
    let first = Char.code s.[0] in
    let rec arcs i cur acc =
      if i >= String.length s then List.rev acc
      else
        let b = Char.code s.[i] in
        let cur = (cur lsl 7) lor (b land 0x7f) in
        if b land 0x80 <> 0 then arcs (i + 1) cur acc else arcs (i + 1) 0 (cur :: acc)
    in
    String.concat "." (List.map string_of_int ((first / 40) :: (first mod 40) :: arcs 1 0 []))

let integer (v : t) : Bignum.t = Bignum.of_bytes v.value
let value_of (v : t) : string = v.value
let bit_string (v : t) : string = if v.value = "" then "" else String.sub v.value 1 (String.length v.value - 1)
let text (v : t) : string = v.value

let time (v : t) : float option =
  let s = v.value in
  let num i k = int_of_string_opt (String.sub s i k) in
  let parts =
    match (v.tag, String.length s) with
    | 0x17, 13 -> Option.map (fun y -> ((if y < 50 then 2000 + y else 1900 + y), 2)) (num 0 2)
    | 0x18, 15 -> Option.map (fun y -> (y, 4)) (num 0 4)
    | _ -> None
  in
  match parts with
  | Some (year, o) -> (
      match (num o 2, num (o + 2) 2, num (o + 4) 2, num (o + 6) 2, num (o + 8) 2) with
      | Some month, Some day, Some hour, Some minute, Some second ->
          Some (Clock.of_local ~offset:0 { Civil.year; month; day } { Clock.hour; minute; second = float_of_int second })
      | _ -> None)
  | None -> None
