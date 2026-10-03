(* St_kernel: the kernel's chunk files (kernel/*.st), embedded by dune
 * at build time, each with its name, in the order St_boot reads them *)

(* the Blue Book's kernel, Smalltalk-80's *)
val files : (string * string) list

(* Squeak's: the Blue Book's, then the files of kernel/squeak/, which
 * add to its classes and change some (closures, St_compile.mli) *)
val squeak : (string * string) list

(* MiniMorphic: Squeak's, then kernel/morphic/MiniMorphic.st -- Morphic
 * in one file (a world, a hand, morphs that step, damage rectangles),
 * to read before the real one *)
val mini_morphic : (string * string) list
