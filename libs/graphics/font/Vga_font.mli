(* Text with a bitmap font: the IBM VGA's 8 by 16 (1987), the PC's text
 * mode. A character is 16 rows of 8 pixels, each row one byte, the
 * leftmost pixel its high bit -- drawing text is copying bits, no lines
 * and no curves (Hershey's way, the other font here): the fastest text
 * there is, and the crispest at its own size, 8 by 16 screen pixels.
 *
 * "A" (0x41), its rows 000010386cc6c6fe c6c6c6c600000000:
 *
 *   00 ........      c6 ##...##.
 *   00 ........      fe #######.
 *   10 ...#....      c6 ##...##.
 *   38 ..###...      ...
 *   6c .##.##..
 *
 * The 256 characters are code page 437's, the PC's: ASCII in the middle,
 * the smileys and arrows below it, and above it the accented letters,
 * the Greek of mathematics and the box-drawing lines Turbo Pascal's
 * windows are made of. A character outside them (a Chinese one) has no
 * glyph: [of_unicode] says None.
 *
 * The font data, fonts/vga8x16.txt, and where it comes from: see
 * fonts/README.md. *)

val width : int (* 8 *)
val height : int (* 16 *)

(* [row c y]: the row [y] (0 at the top) of the character [c] (its code
   page 437 number), a byte *)
val row : int -> int -> int

(* [bit c x y]: whether the pixel (x, y) of the character [c] is set *)
val bit : int -> int -> int -> bool

(* the code page 437 number of a Unicode code point, if it has one *)
val of_unicode : int -> int option

(* [decode s i]: the character of the UTF-8 string [s] at byte [i]: its
   code page 437 number ('?' when it has none), and how many bytes it
   takes. A stray byte is one '?'. *)
val decode : string -> int -> int * int
