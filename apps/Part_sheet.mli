(* A spreadsheet, as a part of a compound document (appkits/embed):
 * TinyExcel's engine and its drawing (Sheet, Sheet_view) behind the
 * four functions a document asks of a part. Active, a click selects a
 * cell and typing goes straight into it -- Enter to put it in, Escape
 * to leave it -- since a part has no formula bar of its own. *)

val kind : string
(* [make ?cols ?rows sheet]: 3 columns and 5 rows unless said, a table
 * in a document; a sheet that is the document asks for more *)
val make : ?cols:int -> ?rows:int -> Sheet.t -> Component.part
val load : string -> Component.part
