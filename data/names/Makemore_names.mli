(* The names: 32,033 first names, in lower case, one a line (the last
 * without its newline), the most frequent first -- emma, olivia, ava,
 * isabella, sophia... -- 26 letters and nothing else, the longest 15
 * letters, 228,145 bytes.
 *
 * It is the [names.txt] of Andrej Karpathy's makemore, taken as it
 * is, because its numbers are known: with a boundary before and after
 * each name it is 228,146 pairs of characters, a table of counts
 * scores 2.454 on it (the average of -log p, natural logarithms), and
 * every lecture after that says what the next idea brought it down
 * to. A model here that reads the same file can be put beside them
 * (Bigram.mli has the first of those numbers, found again).
 *
 * The names are the US Social Security Administration's, public
 * domain; the file from github.com/karpathy/makemore (MIT). *)

val text : string

(* the generator's intermediate name for it *)
val names_txt : string
