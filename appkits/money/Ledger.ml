(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ledger.mli *)

type status = Open | Cleared | Reconciled

type txn = {
  date : Civil.date;
  num : string;
  payee : string;
  category : string;
  memo : string;
  amount : Money.cents;
  status : status;
}

let date_of_string (s : string) : Civil.date option =
  let s = String.trim s in
  (* QIF's 9/14'04: an apostrophe before the year is the 2000s *)
  let s, century = match String.index_opt s '\'' with Some i -> (String.mapi (fun j c -> if j = i then '/' else c) s, 2000) | None -> (s, 1900) in
  match List.map int_of_string_opt (String.split_on_char '/' s) with
  | [ Some month; Some day; Some year ] ->
      let date : Civil.date = { year = (if year < 100 then century + year else year); month; day } in
      if Civil.is_valid date then Some date else None
  | _ -> None

let date_to_string (d : Civil.date) : string =
  if d.year >= 1900 && d.year < 2000 then Printf.sprintf "%d/%d/%02d" d.month d.day (d.year - 1900)
  else Printf.sprintf "%d/%d/%d" d.month d.day d.year

let sort (txns : txn list) : txn list =
  List.stable_sort (fun (a : txn) (b : txn) -> compare (Civil.days_from_civil a.date) (Civil.days_from_civil b.date)) txns

let balances (txns : txn list) : (txn * Money.cents) list =
  let _, lines = List.fold_left (fun (balance, acc) (t : txn) -> (balance + t.amount, (t, balance + t.amount) :: acc)) (0, []) txns in
  List.rev lines

let quickfill (txns : txn list) (prefix : string) : txn option =
  let prefix = String.lowercase_ascii prefix in
  let n = String.length prefix in
  if n = 0 then None
  else
    List.find_opt
      (fun (t : txn) -> String.length t.payee >= n && String.lowercase_ascii (String.sub t.payee 0 n) = prefix)
      (List.rev txns)

let by_category (txns : txn list) : (string * Money.cents) list =
  let add acc (t : txn) =
    let sum = Option.value (List.assoc_opt t.category acc) ~default:0 in
    (t.category, sum + t.amount) :: List.remove_assoc t.category acc
  in
  List.sort compare (List.fold_left add [] txns)

let next_check (txns : txn list) : string =
  match List.filter_map (fun (t : txn) -> int_of_string_opt t.num) txns with
  | [] -> "101"
  | nums -> string_of_int (List.fold_left max 0 nums + 1)
