(* What a network learned, as a file (notes_ai_learning.md section 6).
 *
 * A network that trains in a minute trains in its window. One that
 * takes a night is trained once, by a program of its own, and what it
 * learned is kept: the matrices, by name, and how they were made.
 * The program that uses them embeds the file and never trains.
 *
 * The file is a few lines of text, then the numbers:
 *
 *     weights 1                       the format, and its version
 *     note seed 7                     anything worth remembering:
 *     note games 2000                 a word, then the rest of the line
 *     matrix hidden.w 64 42           a name, rows, columns
 *     matrix hidden.b 64 1
 *     end
 *     ....                            64*42 + 64 numbers, 4 bytes each
 *
 * The numbers are IEEE 754 single precision, little endian, each
 * matrix in the header's order, row by row. Single: a trained weight
 * means nothing past its sixth digit, and the file is half the size.
 * So a matrix read back is not the matrix written, it is its nearest
 * in 32 bits -- 0.1 comes back as 0.100000001490116 -- and writing
 * that one again gives the same bytes.
 *
 * The notes are the point of having a header at all: a weights file
 * that does not say what seed, what data and how many steps made it,
 * and how well it then did, cannot be made again or doubted.
 *
 * Example: one matrix, [[1; -2]], and one note, are 39 bytes of
 * header and 8 of numbers --
 *
 *     "weights 1\nnote seed 7\nmatrix w 1 2\nend\n"
 *     00 00 80 3f    00 00 00 c0          1.0, -2.0
 *)

type t = {
  notes : (string * string) list; (* a word, and what it says *)
  matrices : (string * Matrix.t) list; (* in the file's order; names without spaces *)
}

val to_string : t -> string

(* a file that is not one, or cut short, or with more bytes than its
 * header announces, is refused with the reason *)
val of_string : string -> (t, string) result

val note : t -> string -> string option
val matrix : t -> string -> Matrix.t option
