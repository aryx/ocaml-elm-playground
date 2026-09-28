(* Reconcile: the register balanced against the bank's statement.
 *
 * Once a month the bank says what it has seen: an ending balance.
 * You tick, in the register, each line the statement lists (it is
 * "cleared"); the register's cleared lines, added to what was
 * reconciled last month, must come to the statement's balance. The
 * **difference** is the whole screen:
 *
 *   cleared balance   = reconciled before + the lines ticked now
 *   difference        = statement - cleared balance
 *
 * and zero means done: the ticked lines become reconciled ('R'), and
 * next month starts from them.
 *
 * A difference that is not zero is a mistake, and its size says which
 * one -- the arithmetic people learned doing it by hand, done here for
 * them ([hints]):
 *
 * - a line **not ticked** that the statement has: the difference is
 *   its amount -- or two or three lines' together, a small subset sum
 *   (searched up to three lines);
 * - a line **ticked** that the statement does not have: the
 *   difference is minus its amount;
 * - a line entered with the **wrong sign**, a deposit as a payment:
 *   the difference is minus twice it;
 * - two **digits swapped** in typing, 45.10 for 54.10: the difference
 *   is a multiple of 9 (9.00 here). Always: swapping digits a and b
 *   in places p and p+1 changes a number by (a - b) * 9 * 10^p. So a
 *   line whose amount, with two neighbouring digits swapped, gives the
 *   difference is the suspect; and when none does, the 9 is still
 *   worth saying.
 *
 * Worked example, checked by the tests: a payment of 54.10 typed as
 * 45.10 and ticked. The statement is 9.00 lower than the register
 * ([Transposed], the line and the 54.10 it should say). *)

type hint =
  | Missing of Ledger.txn list (* not ticked, and their amounts are the difference *)
  | Extra of Ledger.txn (* ticked, and not on the statement *)
  | Wrong_sign of Ledger.txn
  | Transposed of Ledger.txn * Money.cents (* the line, and the amount it should say *)
  | Nine (* a multiple of 9 and no line found: look for two digits swapped *)

(* reconciled before: where this month starts *)
val opening : Ledger.txn list -> Money.cents

(* reconciled before, and ticked now *)
val cleared : Ledger.txn list -> Money.cents
val difference : statement:Money.cents -> Ledger.txn list -> Money.cents

(* the explanations the difference allows, the likeliest first; none
 * when it is zero *)
val hints : statement:Money.cents -> Ledger.txn list -> hint list

(* the ticked lines reconciled: this month done *)
val finish : Ledger.txn list -> Ledger.txn list
