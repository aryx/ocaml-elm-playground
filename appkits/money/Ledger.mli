(* Ledger: a checking account's register, the paper one's columns.
 *
 * Quicken's idea (Scott Cook and Tom Proulx, Intuit, 1984) was not an
 * algorithm but a picture: the screen is the register people already
 * kept in their checkbook -- date, check number, payee, payment,
 * deposit, the running balance -- and not an accountant's ledger. So a
 * transaction is one line with one signed amount, and the balance is
 * the sum of the lines above it. (Accountants keep *double entry*,
 * each amount written twice, a debit here and a credit there;
 * QuickBooks, 1992, kept it behind the same register.)
 *
 * Two things the register adds to paper:
 *
 * - **categories**: each line says what the money was for
 *   (Groceries, Utilities, Salary), and a report is the lines summed by
 *   category -- the register is a database whose query is the tax
 *   return ([by_category]);
 * - **QuickFill**: the payee typed is completed from the register
 *   itself, and the rest of the line (category, amount) copied from the
 *   last time you paid them ([quickfill]) -- most of a household's
 *   checks are the same few, every month. *)

(* open, marked cleared while reconciling ('*'), reconciled ('R') *)
type status = Open | Cleared | Reconciled

type txn = {
  date : Civil.date;
  num : string; (* the check's number, or DEP, or nothing *)
  payee : string;
  category : string;
  memo : string;
  amount : Money.cents; (* a payment is negative, a deposit positive *)
  status : status;
}

(* "9/14/84", "9/14/1984", and QIF's "9/14'04" for 2004; two digits
 * are the 1900s *)
val date_of_string : string -> Civil.date option
val date_to_string : Civil.date -> string

(* by date, a day's lines in the order they were written *)
val sort : txn list -> txn list

(* each line with the balance after it, from zero *)
val balances : txn list -> (txn * Money.cents) list

(* [quickfill txns prefix]: the latest line whose payee starts with
 * [prefix], case aside; None for an empty prefix *)
val quickfill : txn list -> string -> txn option

(* the lines summed by category, in the order of the categories'
 * names; the uncategorized under "" *)
val by_category : txn list -> (string * Money.cents) list

(* the check number after the highest one written, or "101" for a new
 * checkbook *)
val next_check : txn list -> string
