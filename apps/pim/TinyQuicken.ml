(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyQuicken: the checkbook on the screen
 * (Scott Cook and Tom Proulx, Intuit, 1984, on DOS).
 *
 * A program with no new algorithm in it, which beat some forty
 * competitors, and it is worth seeing why. They were accounting
 * programs: a chart of accounts, debits and credits, a journal. Cook's
 * observation was that people balancing a checkbook are not
 * accountants, and already have a program they understand, the paper
 * register in the back of the checkbook -- so the screen should be
 * that register, and the check itself. Intuit then watched people use
 * it in their own homes ("follow me home"), and cut whatever made
 * them stop. The ideas are all in what the screen shows:
 *
 *   - **the register** (2): date, number, payee, category, payment,
 *     the C column, deposit, and the balance after each line, as on
 *     paper. Tab or Enter moves to the next field, Enter on the last
 *     records the line; Up and Down pick a recorded line to correct,
 *     the balances below it following;
 *   - **QuickFill**: type the start of a payee and the rest appears,
 *     taken from the register itself; leave the field and the line is
 *     completed from the last time you paid them, category and amount
 *     (Ledger.quickfill). A household writes the same dozen checks
 *     every month;
 *   - **the check** (1): the one on the screen looks like the one in
 *     the drawer, and the amount you type is spelled out beneath it as
 *     you type (Money.words) -- what the bank reads, and what Quicken
 *     printed on the checks, which is where its money came from;
 *   - **reconciling** (3): type the statement's ending balance, tick
 *     with the space bar the lines the statement lists, and watch the
 *     difference; at zero Enter reconciles them. When it is not zero,
 *     the difference is explained -- the line not ticked, the one
 *     ticked by mistake, the sign, the two digits swapped
 *     (Reconcile.mli, and its arithmetic of nines). The opening
 *     register has one: the shoe store's check was typed 45.10, and
 *     the bank's statement, 1,603.89, says what it really was --
 *     correct it in the register, and the difference is gone;
 *   - **categories** (4): the same lines summed by what they were for,
 *     income above expenses -- the report that was the tax return. A
 *     category in brackets is an account, [Checking], and the line a
 *     transfer: money moved, not spent, and not in the report.
 *
 * And money in whole cents (Money.mli: 0.1 + 0.2), and the register
 * saved as QIF (5 and 6, the store's checking.qif), the text format of
 * the Quickens after it.
 *
 * It is drawn as the PC's text screen, as TinyLotus123 is (a Vt fed a
 * Curses screen, drawn by the Teletype way in the CGA's colours).
 *
 * What it uses: appkits/money (Money, Ledger, Reconcile, Qif, tested
 * without a screen in appkits/tests), core's Civil (the dates),
 * libs/terminal's Curses and Vt, the Teletype way's draw_screen. What
 * it does not use: gui/ and the mouse (DOS, 1984), and core's Recur,
 * which scheduled payments would need (an exercise).
 *
 * What it deliberately does not do: printing checks (Quicken's
 * business: its own forms, and the screen for lining the printer up
 * with them), deleting a recorded line, more than one account and
 * transfers between them, split transactions (one check,
 * several categories), budgets, and memorized transactions as a list
 * of their own (QuickFill here remembers by reading the register).
 *
 * Exercises: deleting a line; split transactions; a second account,
 * a savings one, with transfers written as a category in brackets,
 * [Savings], each transfer a line in both registers; scheduled
 * payments over Recur; a budget beside the category report;
 * QuickBooks' double entry behind the same register.
 *)
open Playground

(*****************************************************************************)
(* The keys *)
(*****************************************************************************)

(* the space bar is a key (ticking a line) and also a character typed
   (in a payee); each screen reads the one it wants *)
type key = Char of char | Enter | Escape | Backspace | Tab | Up | Down | Space

let named_keys =
  [ ("Enter", Enter); ("Escape", Escape); ("Backspace", Backspace); ("Tab", Tab); ("ArrowUp", Up); ("ArrowDown", Down); ("space", Space) ]

let keys_of (k : keyboard) ~(before : string list) : key list =
  let now = Set_.elements k.keys in
  let went_down = List.filter (fun n -> not (List.mem n before)) now in
  let chars = List.filter (fun c -> c >= ' ' && c < '\127') (List.init (String.length k.typed) (String.get k.typed)) in
  List.map (fun c -> Char c) chars @ List.filter_map (fun n -> List.assoc_opt n named_keys) went_down

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type screen = Main | Checks | Register | Reconcile | Report

(* a form being filled: its fields by name, in order, and the one
   being typed into *)
type form = { fields : (string * string) list; at : int }

type model = {
  txns : Ledger.txn list;
  screen : screen;
  register : form;
  (* the recorded line the register's form is correcting, if any *)
  editing : int option;
  check : form;
  (* reconciling: the statement's balance typed, then the lines ticked *)
  statement : string;
  ticking : bool;
  selected : int;
  message : string;
  was : string list;
}

let register_fields = [ "Date"; "Num"; "Payee"; "Category"; "Payment"; "Deposit" ]
let check_fields = [ "Date"; "Payee"; "Amount"; "Memo"; "Category" ]
let blank names date = { fields = List.map (fun n -> (n, if n = "Date" then date else "")) names; at = 0 }
let get form name = Option.value (List.assoc_opt name form.fields) ~default:""
let set name v form = { form with fields = List.map (fun (n, x) -> (n, if n = name then v else x)) form.fields }

(* September 1984, and one mistake in it: check 105 was for 54.10 *)
let opening : Ledger.txn list =
  let line ?(num = "") ?(status = Ledger.Open) day payee category amount : Ledger.txn =
    { date = { year = 1984; month = 9; day }; num; payee; category; memo = ""; amount; status }
  in
  [
    (* a transfer into the account, as Quicken wrote the opening *)
    line 1 "Opening Balance" "[Checking]" 100000 ~status:Ledger.Reconciled;
    line 1 "Landlord" "Rent" (-45000) ~num:"101";
    line 3 "Paycheck" "Salary" 125000 ~num:"DEP";
    line 5 "Safeway" "Groceries" (-4217) ~num:"102";
    line 8 "Pacific Gas & Electric" "Utilities" (-6130) ~num:"103";
    line 12 "Safeway" "Groceries" (-3854) ~num:"104";
    line 14 "Shoe Store" "Clothing" (-4510) ~num:"105";
    line 20 "Safeway" "Groceries" (-4000) ~num:"106";
  ]

let last_date txns = match List.rev txns with (t : Ledger.txn) :: _ -> Ledger.date_to_string t.date | [] -> "9/1/84"

let initial =
  {
    txns = opening;
    screen = Register;
    register = blank register_fields (last_date opening);
    editing = None;
    check = blank check_fields (last_date opening);
    statement = "";
    ticking = false;
    selected = 0;
    message = "";
    was = [];
  }

(*****************************************************************************)
(* Forms: typing, QuickFill, recording *)
(*****************************************************************************)

(* the payee completed from the register, and the rest of the line
   from the last time, where it is still empty *)
let quickfill txns ~amounts form =
  match Ledger.quickfill txns (get form "Payee") with
  | None -> form
  | Some t ->
      let fill name v form = if get form name = "" then set name v form else form in
      let form = form |> set "Payee" t.payee |> fill "Category" t.category in
      if List.exists (fun n -> get form n <> "") amounts then form
      else if List.mem "Amount" amounts then set "Amount" (Money.to_string (abs t.amount)) form
      else if t.amount < 0 then set "Payment" (Money.to_string (-t.amount)) form
      else set "Deposit" (Money.to_string t.amount) form

let register_amounts = [ "Payment"; "Deposit" ]
let check_amounts = [ "Amount" ]

(* a key typed into a form: [record] when Enter leaves the last field *)
let type_into txns ~amounts ~record key form model =
  let name = fst (List.nth form.fields form.at) in
  let v = get form name in
  let next form =
    let form = if name = "Payee" then quickfill txns ~amounts form else form in
    if form.at + 1 < List.length form.fields then `Form { form with at = form.at + 1 } else record form
  in
  match key with
  | Char c -> `Form (set name (v ^ String.make 1 c) form)
  | Backspace when v <> "" -> `Form (set name (String.sub v 0 (String.length v - 1)) form)
  | Tab -> next form
  | Enter -> next form
  | Escape -> `Model { model with screen = Main }
  | _ -> `Form form

(* a line made from a form, or why not *)
let make_txn form ~num ~amount : (Ledger.txn, string) result =
  match (Ledger.date_of_string (get form "Date"), amount) with
  | None, _ -> Error "The date should be month/day/year"
  | _, None -> Error "The amount should be a number, like 42.17"
  | Some date, Some amount ->
      Ok { date; num; payee = get form "Payee"; category = get form "Category"; memo = get form "Memo"; amount; status = Ledger.Open }

let record_txn model txn = Ledger.sort (model.txns @ [ txn ])

let record_register model form =
  let amount =
    match (get form "Payment", get form "Deposit") with
    | "", "" -> None
    | p, "" -> Option.map (fun c -> -c) (Money.of_string p)
    | _, d -> Money.of_string d
  in
  match (make_txn form ~num:(get form "Num") ~amount, model.editing) with
  | Ok t, None -> { model with txns = record_txn model t; register = blank register_fields (get form "Date"); message = "Recorded" }
  | Ok t, Some i ->
      (* a correction keeps the line's C column, as a reconciled check
         stays reconciled *)
      let txns = List.mapi (fun j (old : Ledger.txn) -> if j = i then { t with status = old.status } else old) model.txns in
      { model with txns = Ledger.sort txns; editing = None; register = blank register_fields (last_date txns); message = "Corrected" }
  | Error e, _ -> { model with register = form; message = e }

(* Up and Down in the register: a recorded line into the form, or back
   to the new one below the last *)
let pick_line model i =
  match i with
  | Some i ->
      let t = List.nth model.txns i in
      let amount = Money.to_string (abs t.amount) in
      let fields =
        [ ("Date", Ledger.date_to_string t.date); ("Num", t.num); ("Payee", t.payee); ("Category", t.category);
          ("Payment", if t.amount < 0 then amount else ""); ("Deposit", if t.amount >= 0 then amount else "") ]
      in
      { model with editing = Some i; register = { fields; at = model.register.at }; message = "" }
  | None -> { model with editing = None; register = blank register_fields (last_date model.txns) }

let record_check model form =
  let num = Ledger.next_check model.txns in
  match make_txn form ~num ~amount:(Option.map (fun c -> -abs c) (Money.of_string (get form "Amount"))) with
  | Ok t -> { model with txns = record_txn model t; check = blank check_fields (get form "Date"); message = "Check " ^ num ^ " recorded" }
  | Error e -> { model with check = form; message = e }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the lines the statement may list: all but those reconciled before *)
let unreconciled model = List.filter (fun (t : Ledger.txn) -> t.status <> Ledger.Reconciled) model.txns

let tick model =
  let open_lines = unreconciled model in
  match List.nth_opt open_lines model.selected with
  | None -> model
  | Some chosen ->
      let flip (t : Ledger.txn) =
        if t == chosen then { t with status = (if t.status = Ledger.Cleared then Ledger.Open else Ledger.Cleared) } else t
      in
      { model with txns = List.map flip model.txns }

let reconcile key model =
  if not model.ticking then
    match key with
    | Char c -> { model with statement = model.statement ^ String.make 1 c }
    | Backspace when model.statement <> "" -> { model with statement = String.sub model.statement 0 (String.length model.statement - 1) }
    | Enter -> (
        match Money.of_string model.statement with
        | Some _ -> { model with ticking = true; selected = 0; message = "" }
        | None -> { model with message = "The statement's ending balance, like 1,234.56" })
    | Escape -> { model with screen = Main }
    | _ -> model
  else
    let n = List.length (unreconciled model) in
    let statement = Option.value (Money.of_string model.statement) ~default:0 in
    match key with
    | Up -> { model with selected = max 0 (model.selected - 1) }
    | Down -> { model with selected = min (n - 1) (model.selected + 1) }
    | Space -> tick model
    | Enter when Reconcile.difference ~statement model.txns = 0 ->
        { model with txns = Reconcile.finish model.txns; screen = Register; ticking = false; statement = ""; message = "Reconciled" }
    | Enter -> { model with message = "Not balanced yet: the difference should be 0.00" }
    | Escape -> { model with screen = Main; ticking = false }
    | _ -> model

let file = "checking.qif"

let main_menu (caps : < Cap.open_in ; Cap.open_out ; .. >) key model =
  match key with
  | Char '1' -> { model with screen = Checks; message = "" }
  | Char '2' -> { model with screen = Register; message = "" }
  | Char '3' -> { model with screen = Reconcile; statement = ""; ticking = false; message = "" }
  | Char '4' -> { model with screen = Report; message = "" }
  | Char '5' ->
      Playground_platform.store caps file (Qif.write model.txns);
      { model with message = "Saved to " ^ file }
  | Char '6' -> (
      match Option.map Qif.read (Playground_platform.fetch caps file) with
      | Some (Ok txns) -> pick_line { model with txns = Ledger.sort txns; message = "Opened " ^ file } None
      | Some (Error e) -> { model with message = e }
      | None -> { model with message = "No " ^ file ^ " yet: save one with 5" })
  | _ -> model

let press caps key model =
  match model.screen with
  | Main -> main_menu caps key model
  | Register when key = Up -> (
      match model.editing with
      | None -> if model.txns = [] then model else pick_line model (Some (List.length model.txns - 1))
      | Some 0 -> model
      | Some i -> pick_line model (Some (i - 1)))
  | Register when key = Down -> (
      match model.editing with
      | None -> model
      | Some i -> pick_line model (if i + 1 < List.length model.txns then Some (i + 1) else None))
  | Register -> (
      match type_into model.txns ~amounts:register_amounts ~record:(fun f -> `Model (record_register model f)) key model.register model with
      | `Form register -> { model with register; message = "" }
      | `Model m -> m)
  | Checks -> (
      match type_into model.txns ~amounts:check_amounts ~record:(fun f -> `Model (record_check model f)) key model.check model with
      | `Form check -> { model with check; message = "" }
      | `Model m -> m)
  | Reconcile -> reconcile key model
  | Report -> ( match key with Escape -> { model with screen = Main } | _ -> model)

let update caps computer model =
  let keys = keys_of computer.keyboard ~before:model.was in
  let model = List.fold_left (fun m k -> press caps k m) model keys in
  { model with was = Set_.elements computer.keyboard.keys }

(*****************************************************************************)
(* The text screen *)
(*****************************************************************************)

let width = 80
let height = 25
let paper = { Vt.plain with fg = Vt.White; bg = Vt.Blue }
let title = { paper with fg = Vt.Yellow; bold = true }
let lit = { paper with reverse = true }
let dim = { paper with fg = Vt.Cyan }

let put ?(attrs = paper) row col s t = Curses.put ~attrs row col s t
let right w s = if String.length s >= w then s else String.make (w - String.length s) ' ' ^ s
let left w s = if String.length s >= w then String.sub s 0 w else s ^ String.make (w - String.length s) ' '

(* the screen's frame: the blue, the title, the keys at the bottom *)
let frame name keys =
  let t = Curses.create ~rows:height ~cols:width in
  let t = List.fold_left (fun t r -> put r 0 (String.make width ' ') t) t (List.init height Fun.id) in
  t |> put ~attrs:lit 0 0 (left width (" " ^ name)) |> put ~attrs:dim 23 1 keys

(* a form's field drawn at a place and a width: lit when it is being
   typed into, the cursor after its text, QuickFill's completion dim *)
let field txns form name ~row ~col ~w ?(align = left) (t, cursor) =
  let v = get form name in
  let editing = fst (List.nth form.fields form.at) = name in
  let t = put ?attrs:(if editing then Some lit else None) row col (align w v) t in
  if not editing then (t, cursor)
  else
    let t =
      match (name, Ledger.quickfill txns v) with
      | "Payee", Some q when String.length q.payee > String.length v ->
          let rest = String.sub q.payee (String.length v) (String.length q.payee - String.length v) in
          put ~attrs:{ lit with fg = Vt.Cyan } row (col + String.length v) (left (w - String.length v) rest) t
      | _ -> t
    in
    (t, Some (row, col + min (String.length v) (w - 1)))

(* the register's columns: 80 characters, as the paper's *)
let register_row (t : Ledger.txn) balance =
  let c = match t.status with Ledger.Open -> " " | Ledger.Cleared -> "*" | Ledger.Reconciled -> "R" in
  let pay = if t.amount < 0 then Money.to_string (-t.amount) else "" in
  let dep = if t.amount >= 0 then Money.to_string t.amount else "" in
  String.concat " " [ left 8 (Ledger.date_to_string t.date); left 4 t.num; left 20 t.payee; left 12 t.category; right 9 pay; c; right 9 dep; right 10 (Money.to_string balance) ]

let register_screen model =
  let t = frame "Register    Checking" "Tab: next field   Enter on Deposit: record   Up, Down: correct   Esc: Menu" in
  let t = put ~attrs:title 2 0 "DATE     NUM  PAYEE                CATEGORY       PAYMENT C   DEPOSIT    BALANCE" t in
  let t = put 3 0 (String.make width '-') t in
  let all = Ledger.balances model.txns in
  (* the last lines, or from the one being corrected *)
  let shown = 16 in
  let first = max 0 (List.length all - shown) in
  let first = match model.editing with Some i when i < first -> i | _ -> first in
  let lines = List.filteri (fun i _ -> i >= first && i < first + shown) all in
  let t = List.fold_left (fun t (i, (txn, b)) -> put (4 + i) 0 (register_row txn b) t) t (List.mapi (fun i l -> (i, l)) lines) in
  let row = match model.editing with Some i -> 4 + i - first | None -> 4 + List.length lines in
  (* the form drawn over the line it corrects, its C and balance kept *)
  let t = put row 0 (String.make 58 ' ') t in
  let f = model.register in
  let tc =
    (t, None)
    |> field model.txns f "Date" ~row ~col:0 ~w:8
    |> field model.txns f "Num" ~row ~col:9 ~w:4
    |> field model.txns f "Payee" ~row ~col:14 ~w:20
    |> field model.txns f "Category" ~row ~col:35 ~w:12
    |> field model.txns f "Payment" ~row ~col:48 ~w:9 ~align:right
    |> field model.txns f "Deposit" ~row ~col:60 ~w:9 ~align:right
  in
  let balance = match List.rev all with (_, b) :: _ -> b | [] -> 0 in
  let t, cursor = tc in
  (put ~attrs:title 21 50 ("Ending Balance: " ^ right 12 (Money.to_string balance)) t, cursor)

let check_screen model =
  let t = frame "Write Checks    Checking" "Tab, Enter: next field    Enter on Category: record the check    Esc: Main Menu" in
  let f = model.check in
  let amount = Option.value (Money.of_string (get f "Amount")) ~default:0 in
  (* the check, in the box characters draw_screen turns into lines *)
  let bar = String.concat "" (List.init 74 (fun _ -> "\u{2500}")) in
  let t = put 3 2 ("\u{250C}" ^ bar ^ "\u{2510}") t |> put 13 2 ("\u{2514}" ^ bar ^ "\u{2518}") in
  let t = List.fold_left (fun t r -> t |> put r 2 "\u{2502}" |> put r 77 "\u{2502}") t (List.init 9 (fun i -> 4 + i)) in
  let t =
    t
    |> put 4 64 ("No. " ^ Ledger.next_check model.txns)
    |> put 5 52 "Date"
    |> put 7 4 "Pay to the"
    |> put 8 4 "Order of" |> put 8 14 (String.make 44 '_') |> put 8 60 "$"
    |> put 10 4 (left 62 (Money.words amount ^ " " ^ String.make 62 '*'))
    |> put 10 67 "Dollars"
    |> put 12 4 "Memo" |> put 12 10 (String.make 30 '_') |> put 12 48 (String.make 26 '_')
    |> put 15 4 "Category"
  in
  let t, cursor =
    (t, None)
    |> field model.txns f "Date" ~row:5 ~col:58 ~w:10
    |> field model.txns f "Payee" ~row:8 ~col:14 ~w:44
    |> field model.txns f "Amount" ~row:8 ~col:62 ~w:12 ~align:right
    |> field model.txns f "Memo" ~row:12 ~col:10 ~w:30
    |> field model.txns f "Category" ~row:15 ~col:14 ~w:20
  in
  (t, cursor)

let hint_text (h : Reconcile.hint) : string =
  let line (t : Ledger.txn) = Printf.sprintf "%s %s %s" t.num t.payee (Money.to_string t.amount) in
  match h with
  | Reconcile.Missing ts -> "On the statement, not marked yet? " ^ String.concat ", " (List.map line ts)
  | Reconcile.Extra t -> "Marked, and not on the statement? " ^ line t
  | Reconcile.Wrong_sign t -> "Entered with the wrong sign? " ^ line t
  | Reconcile.Transposed (t, a) -> Printf.sprintf "Two digits swapped? %s: the statement may say %s" (line t) (Money.to_string a)
  | Reconcile.Nine -> "The difference is a multiple of 9: two digits swapped somewhere?"

let reconcile_screen model =
  let t = frame "Reconcile    Checking" "Space: mark cleared   Up, Down   Enter at a difference of 0.00   Esc: Main Menu" in
  if not model.ticking then
    let q = "Bank statement ending balance: " in
    let t = put 3 4 "Opening balance (reconciled before):" t |> put 3 44 (right 12 (Money.to_string (Reconcile.opening model.txns))) in
    let t = put 5 4 q t |> put ~attrs:lit 5 (4 + String.length q) (left 12 model.statement) in
    (t, Some (5, 4 + String.length q + String.length model.statement))
  else
    let statement = Option.value (Money.of_string model.statement) ~default:0 in
    let t = put ~attrs:title 2 0 "   DATE     NUM  PAYEE                         AMOUNT" t in
    let lines = unreconciled model in
    let t =
      List.fold_left
        (fun t (i, (x : Ledger.txn)) ->
          let mark = if x.status = Ledger.Cleared then "*" else " " in
          let s = Printf.sprintf " %s %s %s %s %s" mark (left 8 (Ledger.date_to_string x.date)) (left 4 x.num) (left 20 x.payee) (right 12 (Money.to_string x.amount)) in
          put ?attrs:(if i = model.selected then Some lit else None) (3 + i) 0 (left 52 s) t)
        t
        (List.mapi (fun i x -> (i, x)) lines)
    in
    let diff = Reconcile.difference ~statement model.txns in
    let money row label v t = t |> put row 4 label |> put row 44 (right 12 (Money.to_string v)) in
    let t =
      t
      |> money 14 "Cleared balance" (Reconcile.cleared model.txns)
      |> money 15 "Bank statement ending balance" statement
      |> money 16 "Difference" diff
      |> put ~attrs:title 16 60 (if diff = 0 then "Balanced!" else "")
    in
    let t = List.fold_left (fun t (i, h) -> put (18 + i) 4 (hint_text h) t) t (List.mapi (fun i h -> (i, h)) (Reconcile.hints ~statement model.txns)) in
    (t, None)

let report_screen model =
  let t = frame "Report    Categories" "Esc: Main Menu" in
  (* transfers, [Checking], are money moved rather than earned or spent *)
  let transfer c = String.length c > 0 && c.[0] = '[' in
  let totals =
    List.filter_map (fun (c, v) -> if transfer c then None else Some ((if c = "" then "(none)" else c), v)) (Ledger.by_category model.txns)
  in
  let biggest = List.fold_left (fun m (_, v) -> max m (abs v)) 1 totals in
  let section row name keep t =
    let lines = List.filter (fun (_, v) -> keep v) totals in
    let t = put ~attrs:title row 2 name t in
    let t =
      List.fold_left
        (fun t (i, (c, v)) ->
          (* a bar of reversed spaces: the only graphics a text screen has *)
          let bar = max 1 (abs v * 40 / biggest) in
          t |> put (row + 1 + i) 4 (left 16 c) |> put (row + 1 + i) 20 (right 12 (Money.to_string (abs v))) |> put ~attrs:lit (row + 1 + i) 34 (String.make bar ' '))
        t
        (List.mapi (fun i l -> (i, l)) lines)
    in
    (t, row + List.length lines + 2)
  in
  let t, row = section 2 "INCOME" (fun v -> v > 0) t in
  let t, row = section row "EXPENSES" (fun v -> v < 0) t in
  let net = List.fold_left (fun s (_, v) -> s + v) 0 totals in
  (put ~attrs:title row 4 (left 16 "NET" ^ right 12 (Money.to_string net)) t, None)

let main_screen () =
  let t = frame "Quicken    Main Menu" "Type a number" in
  let items = [ "1. Write Checks"; "2. Register"; "3. Reconcile"; "4. Reports"; "5. Save to " ^ file; "6. Open " ^ file ] in
  (List.fold_left (fun t (i, s) -> put (6 + (2 * i)) 30 s t) t (List.mapi (fun i s -> (i, s)) items), None)

let screen model : Curses.t =
  let t, cursor =
    match model.screen with
    | Main -> main_screen ()
    | Checks -> check_screen model
    | Register -> register_screen model
    | Reconcile -> reconcile_screen model
    | Report -> report_screen model
  in
  t |> put ~attrs:title 24 1 model.message |> Curses.cursor cursor

let view (computer : computer) model =
  let vt = Vt.feed (Vt.create ~rows:height ~cols:width) (Curses.redraw (screen model)) in
  rectangle black computer.screen.width computer.screen.height :: Teletype.draw_screen ~pc:true ~cursor:true computer vt

let app caps = game view (update caps) initial
let main = Program.main __MODULE__ (fun () -> Cap.main (fun caps -> Playground_platform.run_app (app (caps :> < Cap.open_in ; Cap.open_out >))))
