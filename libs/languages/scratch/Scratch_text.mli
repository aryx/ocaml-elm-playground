(* Scratch as text: the scratchblocks notation, which the Scratch
 * forums and wiki have used since 2013 to write blocks where only text
 * goes (scratchblocks, Tim Radvan, "blob8108"). A block is a line of
 * its words, its slots in brackets that say their shape:
 *
 *   (10)  a number,  [Hello!]  a text,  [space v]  a menu,
 *   <mouse down?>  a predicate,  (x position)  a reporter,
 *   (score)  a variable -- a round thing that is no reporter's words
 *
 * a C block's mouth indented under it and closed by "end" (and split
 * by "else"), a blank line between two scripts:
 *
 *   when flag clicked
 *   forever
 *     move (10) steps
 *     if <touching [edge v]?> then
 *       turn right (180) degrees
 *     end
 *   end
 *
 * Reading it is matching: a line's words and bracketed slots against
 * each spec's template (Scratch_blocks), the brackets read inside out.
 * The one ambiguity is < and >, which are brackets and also the
 * comparisons: a < opens a predicate unless a space follows it, a >
 * closes one unless a space precedes it -- so (a) < (b) compares and
 * <(a) < (b)> is a predicate that does.
 *
 * Worked example: "move (10) steps" is the block motion_movesteps with
 * the argument Lit "10"; "say (join [hi ] (score))" is looks_say whose
 * argument is the block operator_join, itself with Lit "hi " and the
 * variable score. Printing gives the text back. *)

(* the scripts of a text, each at (0, 0); the error says the line *)
val parse : string -> (Scratch_blocks.script list, string) result

val print : Scratch_blocks.script list -> string

(* one block's own line (its mouths not included) *)
val line : Scratch_blocks.block -> string
