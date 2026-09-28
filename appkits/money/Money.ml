(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Money.mli *)

type cents = int

let digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let of_string (s : string) : cents option =
  let s = String.concat "" (String.split_on_char ',' (String.trim s)) in
  let negative, s = if s <> "" && s.[0] = '-' then (true, String.sub s 1 (String.length s - 1)) else (false, s) in
  let s = if s <> "" && s.[0] = '$' then String.sub s 1 (String.length s - 1) else s in
  let whole, fraction =
    match String.split_on_char '.' s with
    | [ w ] -> (w, "")
    | [ w; f ] -> (w, f)
    | _ -> ("", "x")
  in
  let whole = if whole = "" && fraction <> "" then "0" else whole in
  if digits whole && (fraction = "" || (digits fraction && String.length fraction <= 2)) then
    let fraction = fraction ^ String.make (2 - String.length fraction) '0' in
    let c = (int_of_string whole * 100) + int_of_string fraction in
    Some (if negative then -c else c)
  else None

let to_string (c : cents) : string =
  let a = abs c in
  let whole = string_of_int (a / 100) in
  (* a comma every three digits, counted from the right *)
  let n = String.length whole in
  let buf = Buffer.create (n + 4) in
  String.iteri
    (fun i ch ->
      if i > 0 && (n - i) mod 3 = 0 then Buffer.add_char buf ',';
      Buffer.add_char buf ch)
    whole;
  Printf.sprintf "%s%s.%02d" (if c < 0 then "-" else "") (Buffer.contents buf) (a mod 100)

let ones =
  [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"; "eleven"; "twelve";
     "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen" |]

let tens = [| ""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |]

(* 0 to 999: "one hundred twenty-three" *)
let below_thousand n =
  let hundreds = n / 100 and rest = n mod 100 in
  let rest_words =
    if rest < 20 then ones.(rest)
    else if rest mod 10 = 0 then tens.(rest / 10)
    else tens.(rest / 10) ^ "-" ^ ones.(rest mod 10)
  in
  String.concat " " (List.filter (( <> ) "") [ (if hundreds > 0 then ones.(hundreds) ^ " hundred" else ""); rest_words ])

(* in groups of three digits, each with its scale's name *)
let rec in_words n =
  let scales = [ (1_000_000_000, "billion"); (1_000_000, "million"); (1_000, "thousand") ] in
  match List.find_opt (fun (s, _) -> n >= s) scales with
  | Some (s, name) ->
      let rest = n mod s in
      in_words (n / s) ^ " " ^ name ^ if rest > 0 then " " ^ in_words rest else ""
  | None -> below_thousand n

let words (c : cents) : string =
  let c = abs c in
  let dollars = if c / 100 = 0 then "zero" else in_words (c / 100) in
  Printf.sprintf "%s%s and %02d/100" (String.uppercase_ascii (String.sub dollars 0 1)) (String.sub dollars 1 (String.length dollars - 1)) (c mod 100)
