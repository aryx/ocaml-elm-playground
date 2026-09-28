(* Ps_disk: PostScript programs to run, each a page, after the Blue
 * Book's way of teaching the language (Adobe, "PostScript Language
 * Tutorial and Cookbook", 1985): a star turned by rotate, a word
 * turned twelve times, a tree drawn by a recursive procedure, a pie
 * chart from an array, a Bezier curve and its control points, and
 * the stack used as a calculator. Each is checked by the tests to run
 * to its end, and the calculator to print what it should. *)

(* the programs, by name, in the order shown *)
val programs : (string * string) list
