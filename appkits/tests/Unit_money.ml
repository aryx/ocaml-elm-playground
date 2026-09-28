(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/money: Money, Ledger, Reconcile, Qif *)

let t = Testo.create

let txn ?(num = "") ?(category = "") ?(status = Ledger.Open) day payee amount : Ledger.txn =
  { date = { year = 1984; month = 9; day }; num; payee; category; memo = ""; amount; status }

(* the reason for cents, and the two directions *)
let test_cents () =
  Alcotest.(check bool) "floats cannot add a tenth and two" false (0.1 +. 0.2 = 0.3);
  Alcotest.(check bool) "cents can" true (10 + 20 = 30);
  let check s expected = Alcotest.(check (option int)) s expected (Money.of_string s) in
  check "1,234.56" (Some 123456);
  check "-42.17" (Some (-4217));
  check "$42" (Some 4200);
  check "42.5" (Some 4250);
  check ".07" (Some 7);
  check "1.234" None;
  check "twelve" None;
  Alcotest.(check string) "commas" "1,234,567.89" (Money.to_string 123456789);
  Alcotest.(check string) "negative" "-42.17" (Money.to_string (-4217));
  Alcotest.(check string) "cents only" "0.07" (Money.to_string 7)

(* Money.mli's worked example, and the edges of the groups *)
let test_words () =
  let check c expected = Alcotest.(check string) expected expected (Money.words c) in
  check 12345 "One hundred twenty-three and 45/100";
  check 7 "Zero and 07/100";
  check 121500 "One thousand two hundred fifteen and 00/100";
  check 100000000 "One million and 00/100";
  check 4000 "Forty and 00/100";
  check 1900 "Nineteen and 00/100"

let test_ledger () =
  let txns = [ txn 1 "Safeway" (-4217) ~category:"Groceries"; txn 3 "Pay" 150000 ~category:"Salary"; txn 8 "Safe Harbor" (-900) ] in
  Alcotest.(check (option string)) "QuickFill: the latest that starts so" (Some "Safe Harbor")
    (Option.map (fun (t : Ledger.txn) -> t.payee) (Ledger.quickfill txns "safe"));
  Alcotest.(check (option string)) "and the rest of the line with it" (Some "Groceries")
    (Option.map (fun (t : Ledger.txn) -> t.category) (Ledger.quickfill txns "safew"));
  Alcotest.(check (list int)) "running balance" [ -4217; 145783; 144883 ] (List.map snd (Ledger.balances txns));
  Alcotest.(check (list (pair string int))) "by category" [ ("", -900); ("Groceries", -4217); ("Salary", 150000) ] (Ledger.by_category txns);
  Alcotest.(check string) "next check" "102" (Ledger.next_check [ txn 1 "A" (-1) ~num:"101"; txn 2 "B" 1 ~num:"DEP" ]);
  Alcotest.(check (option string)) "QIF's date of the 2000s" (Some "9/14/2004")
    (Option.map Ledger.date_to_string (Ledger.date_of_string "9/14'04"));
  Alcotest.(check (option string)) "no 31st of September" None (Option.map Ledger.date_to_string (Ledger.date_of_string "9/31/84"))

(* Reconcile.mli's worked examples, one per mistake *)
let test_reconcile () =
  let opening = txn 1 "Opening Balance" 100000 ~status:Ledger.Reconciled in
  let rent = txn 2 "Landlord" (-50000) ~status:Ledger.Cleared in
  let shoes = txn 3 "Shoe store" (-5410) ~status:Ledger.Cleared in
  let gas = txn 4 "Gas" (-2000) in
  let books = txn 5 "Books" (-1500) in
  let statement = 100000 - 50000 - 5410 in
  Alcotest.(check int) "balanced" 0 (Reconcile.difference ~statement [ opening; rent; shoes; gas ]);
  Alcotest.(check int) "opening" 100000 (Reconcile.opening [ opening; rent ]);
  let hints ~statement txns = Reconcile.hints ~statement txns in
  (* the bank has the gas and the books, unticked *)
  (match hints ~statement:(statement - 3500) [ opening; rent; shoes; gas; books ] with
  | Reconcile.Missing [ a; b ] :: _ -> Alcotest.(check (list string)) "missing, two together" [ "Gas"; "Books" ] [ a.payee; b.payee ]
  | _ -> Alcotest.fail "the two unticked lines");
  (* the rent ticked, and the bank has not seen it *)
  (match hints ~statement:(statement + 50000) [ opening; rent; shoes ] with
  | Reconcile.Extra t :: _ -> Alcotest.(check string) "extra" "Landlord" t.payee
  | _ -> Alcotest.fail "the ticked rent");
  (* a refund of 20.00 entered as a payment *)
  let refund = txn 6 "Refund" (-2000) ~status:Ledger.Cleared in
  (match hints ~statement:(statement + 2000) [ opening; rent; shoes; refund ] with
  | [ Reconcile.Wrong_sign t ] -> Alcotest.(check string) "wrong sign" "Refund" t.payee
  | _ -> Alcotest.fail "the refund's sign");
  (* 54.10 typed as 45.10: the bank took 9.00 more *)
  let typo = { shoes with amount = -4510 } in
  (match hints ~statement [ opening; rent; typo ] with
  | [ Reconcile.Transposed (t, a) ] -> Alcotest.(check (pair string int)) "transposed" ("Shoe store", -5410) (t.payee, a)
  | _ -> Alcotest.fail "the swapped digits");
  Alcotest.(check bool) "finished: ticked become reconciled" true
    (List.for_all (fun (t : Ledger.txn) -> t.status <> Ledger.Cleared) (Reconcile.finish [ opening; rent; shoes ]))

let test_qif () =
  let txns =
    [ txn 1 "Opening Balance" 100000 ~status:Ledger.Reconciled; txn 2 "Safeway" (-4217) ~num:"101" ~category:"Groceries" ~status:Ledger.Cleared;
      { (txn 3 "Pay" 150000 ~category:"Salary") with memo = "September" } ]
  in
  Alcotest.(check bool) "written and read back" true (Qif.read (Qif.write txns) = Ok txns);
  let text = "!Type:Bank\nD9/14'04\nT-1,234.50\nPLandlord\nXunknown\n^\n" in
  (match Qif.read text with
  | Ok [ t ] -> Alcotest.(check (pair string int)) "a record, an unknown field skipped" ("Landlord", -123450) (t.payee, t.amount)
  | _ -> Alcotest.fail "one record");
  Alcotest.(check bool) "no date, an error" true (Result.is_error (Qif.read "!Type:Bank\nT12\n^\n"))

let tests =
  Testo.categorize "money"
    [
      t "cents" test_cents; t "words" test_words; t "ledger" test_ledger; t "reconcile" test_reconcile; t "qif" test_qif;
    ]
