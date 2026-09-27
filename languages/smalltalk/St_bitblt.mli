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
   destination's; this one goes a pixel at a time, which is the same
   result, far more slowly: the word at a time is an exercise.

   A BitBlt's fields: destForm sourceForm halftoneForm combinationRule
   destX destY width height sourceX sourceY clipX clipY clipWidth
   clipHeight. *)

type oop = St_memory.oop

(* the primitive 96, copyBits: false if a field is not what it should *)
val copy_bits : St_memory.t -> oop -> bool

(* how many copyBits since the start: the host redraws the Display when
 * it changed *)
val changes : unit -> int

(* a Form's width, height and pixels (true black), for the host *)
val form : St_memory.t -> oop -> (int * int * (int -> int -> bool)) option
