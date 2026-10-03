(* St_bitblt: Form and BitBlt, the one drawing primitive (Blue Book,
   chapter 18; Dan Ingalls, "The Smalltalk Graphics Kernel", Byte,
   August 1981).

   A Form is a picture of one bit per pixel, 1 black: its fields are
   its bits (a ByteArray, each row padded to 16 bits, as the Alto's
   words, most significant bit leftmost), its width and its height.

   BitBlt ("bit block transfer") copies a rectangle of a source Form
   onto a destination Form, combining each source bit s with the
   destination's bit d by one of 16 rules -- all the functions of two
   bits, the rule's number their truth table read from (s, d) = (0, 0):

     rule 0  0        clear         rule 3  s        store
     rule 1  s and d                rule 6  s xor d  reverse
     rule 4  not s and d  erase     rule 7  s or d   paint (under)
     rule 15 1        fill          ...

   the result's bit being (rule >> (3 - (2 s + d))) & 1. Before that,
   the source is and-ed with a halftone, a 16 by 16 pattern aligned on
   the destination (a gray); no source means all ones, no halftone
   too. And the whole is clipped to a rectangle.

   Everything the Smalltalk-80 display did -- text, windows, lines,
   scrolling, the cursor -- was this one primitive, called with
   different Forms and rules. Ingalls's version worked a 16-bit word
   at a time, shifting and masking the source's words onto the
   destination's. Here the definition is written a pixel at a time
   ([blit ~simple:true]), and what runs is the same a byte at a time,
   eight pixels by one and, or and not: the source's bits shifted to
   line up with the destination's bytes, a mask at each end of a row.
   A test checks that the two agree, on every rule.

   A BitBlt's fields: destForm sourceForm halftoneForm combinationRule
   destX destY width height sourceX sourceY clipX clipY clipWidth
   clipHeight. *)

type oop = St_memory.oop

(* a Form's bits (each row [stride] bytes), width and height *)
type form = { bits : Bytes.t; w : int; h : int; stride : int }

(* [blit ~dest ~source ~halftone ~rule ~dx ~dy ~sx ~sy (x0, y0, x1, y1)]:
 * the destination's pixels of the rectangle (x0 and y0 in, x1 and y1
 * out; inside the Form), each combined with the source's pixel that
 * (sx, sy) puts on (dx, dy); no source, all ones. A Form may be its own
 * source. [~simple]: a pixel at a time, the definition. *)
val blit :
  ?simple:bool ->
  dest:form ->
  source:form option ->
  halftone:form option ->
  rule:int ->
  dx:int ->
  dy:int ->
  sx:int ->
  sy:int ->
  int * int * int * int ->
  unit

(* the primitive 96, copyBits: false if a field is not what it should *)
val copy_bits : St_memory.t -> oop -> bool

(* how many copyBits since the start: the host redraws the Display when
 * it changed *)
val changes : unit -> int

(* a Form's width, height and pixels (true black), for the host *)
val form : St_memory.t -> oop -> (int * int * (int -> int -> bool)) option
