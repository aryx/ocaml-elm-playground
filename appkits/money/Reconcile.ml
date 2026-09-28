(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Reconcile.mli *)

type hint =
  | Missing of Ledger.txn list
  | Extra of Ledger.txn
  | Wrong_sign of Ledger.txn
  | Transposed of Ledger.txn * Money.cents
  | Nine

let sum_of status txns = List.fold_left (fun s (t : Ledger.txn) -> if List.mem t.status status then s + t.amount else s) 0 txns
let opening txns = sum_of [ Ledger.Reconciled ] txns
let cleared txns = sum_of [ Ledger.Reconciled; Ledger.Cleared ] txns
let difference ~statement txns = statement - cleared txns

(* the amounts [a] becomes with two neighbouring digits swapped *)
let swaps (a : Money.cents) : Money.cents list =
  let s = string_of_int (abs a) in
  List.filter_map
    (fun i ->
      if s.[i] = s.[i + 1] then None
      else
        let b = Bytes.of_string s in
        Bytes.set b i s.[i + 1];
        Bytes.set b (i + 1) s.[i];
        let v = int_of_string (Bytes.to_string b) in
        Some (if a < 0 then -v else v))
    (List.init (max 0 (String.length s - 1)) Fun.id)

(* up to three of [items] whose amounts add up to [target], the
   fewest first *)
let subset (items : Ledger.txn list) (target : Money.cents) : Ledger.txn list option =
  let rec pick k start sum chosen =
    if k = 0 then if sum = target then Some (List.rev chosen) else None
    else
      let rec from = function
        | [] -> None
        | (t : Ledger.txn) :: rest -> (
            match pick (k - 1) rest (sum + t.amount) (t :: chosen) with Some c -> Some c | None -> from rest)
      in
      from start
  in
  List.fold_left (fun found k -> match found with Some _ -> found | None -> pick k items 0 []) None [ 1; 2; 3 ]

let hints ~statement (txns : Ledger.txn list) : hint list =
  let diff = difference ~statement txns in
  if diff = 0 then []
  else
    let open_ = List.filter (fun (t : Ledger.txn) -> t.status = Ledger.Open) txns in
    let ticked = List.filter (fun (t : Ledger.txn) -> t.status = Ledger.Cleared) txns in
    let missing = match subset open_ diff with Some ts -> [ Missing ts ] | None -> [] in
    let extra = List.filter_map (fun (t : Ledger.txn) -> if t.amount = -diff then Some (Extra t) else None) ticked in
    let sign = List.filter_map (fun (t : Ledger.txn) -> if 2 * t.amount = -diff then Some (Wrong_sign t) else None) ticked in
    let transposed =
      List.concat_map
        (fun (t : Ledger.txn) -> List.filter_map (fun a -> if a - t.amount = diff then Some (Transposed (t, a)) else None) (swaps t.amount))
        ticked
    in
    let nine = if transposed = [] && diff mod 9 = 0 then [ Nine ] else [] in
    missing @ extra @ sign @ transposed @ nine

let finish (txns : Ledger.txn list) : Ledger.txn list =
  List.map (fun (t : Ledger.txn) -> if t.status = Ledger.Cleared then { t with status = Ledger.Reconciled } else t) txns
