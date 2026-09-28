(* Qif: the Quicken Interchange Format, the text in which personal
 * finance programs and banks exchanged a register (later Quickens,
 * Microsoft Money, and banks' "download your statement" before OFX).
 *
 * A line per field, its first character saying which, a record ended
 * by ^, after a header naming the kind of account:
 *
 *   !Type:Bank
 *   D9/14/84          date
 *   T-42.17           amount, a payment negative
 *   N101              number
 *   PSafeway          payee
 *   LGroceries        category
 *   MFriday's         memo
 *   C*                cleared, a star; reconciled, X
 *   ^
 *
 * Small enough to read in an evening, and it shows what a format with
 * no schema costs: unknown letters are skipped, a missing date is an
 * error, and nothing says which of the world's date orders was meant
 * (month first, here, as Quicken wrote it). TinyQuicken's file. *)

val write : Ledger.txn list -> string

(* the records of a bank register; Error names the first record that
 * has no valid date or amount *)
val read : string -> (Ledger.txn list, string) result
