(* What you can type into a cell, and what it means.
 *
 *   42            a number
 *   hello         text
 *   =A1+B2*2      a formula: arithmetic over other cells
 *   =SUM(A1:A9)   a function over a range of them
 *
 * The grammar, which is the one every spreadsheet has had since
 * VisiCalc (Dan Bricklin and Bob Frankston, 1979), written here as
 * the parser reads it:
 *
 *   expr    ::= term (('+' | '-') term)*
 *   term    ::= factor (('*' | '/') factor)*
 *   factor  ::= '-'? atom
 *   atom    ::= number | ref (':' ref)? | name '(' args ')' | '(' expr ')'
 *   ref     ::= letters digits          A1, B12, AA3
 *
 * Two rules in four lines, and they are what gives multiplication its
 * precedence over addition: a sum is made of products, so the parser
 * that reads a sum asks for products, and a product binds tighter by
 * being lower down. That is **recursive descent** -- one function per
 * rule, calling the function below it -- and it is the parser to know
 * first, because the grammar and the code are the same shape.
 *
 * Worked example, "=2+3*A1" with A1 holding 4:
 *
 *   expr -> term(2) '+' term(3*A1)
 *                         -> factor(3) '*' factor(A1)
 *   value = 2 + (3 * 4) = 14, and not (2+3)*4 = 20
 *
 * A1 is a *reference*, not a value: the formula says which cell to
 * read, and Sheet is what knows the answer and what to do when it
 * changes. That separation is the whole of why a spreadsheet can
 * recalculate (Sheet.mli).
 *
 * What it deliberately does not have: strings in formulas, comparison
 * and IF, absolute references ($A$1, which matter when a formula is
 * copied), sheets other than this one, and the hundreds of functions
 * a real one carries. It has the five that show what a function over
 * a range is: SUM, PRODUCT, MIN, MAX, AVERAGE, and COUNT. *)

(* where a cell is: its column and row, both counted from 0, so A1 is
 * (0, 0) *)
type cell = int * int

type expr =
  | Number of float
  | Ref of cell
  | Range of cell * cell
  | Unary of char * expr
  | Binop of char * expr * expr
  | Call of string * expr list

(* What a cell holds, before anything is computed: [Formula] when it
 * starts with '=', [Value] for a number, [Text] for anything else --
 * which is how a spreadsheet tells "3" from "three" without asking.
 *
 * [Invalid] is the fifth, and it is there rather than as a failure
 * because of what a spreadsheet has to do with a formula that does
 * not parse: keep it. The text stays in the cell, to be corrected,
 * and the cell shows an error -- so "it does not parse" is a thing a
 * cell can hold, and not a reason to refuse what was typed. *)
type content =
  | Formula of expr
  | Invalid of string
  | Value of float
  | Text of string
  | Blank

(* [content_of s]: what typing [s] into a cell means. Anything can be
 * typed into a cell, so this cannot fail. *)
val content_of : string -> content

(* [parse s]: the formula in [s] (without its leading '='), for the
 * tests and for anybody wanting the tree *)
val parse : string -> (expr, string) result

(* which cells an expression reads, a range counted as every cell in
 * it: what Sheet builds its dependency graph out of *)
val refs : expr -> cell list

(* "A1", "BC12": how a cell is written and read. A column is base 26
 * with no zero, which is why the column after Z is AA and not BA. *)
val name_of_cell : cell -> string
val cell_of_name : string -> cell option
