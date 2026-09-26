(* St_kernel: the kernel's chunk files (kernel/*.st), embedded by dune
 * at build time, each with its name, in the order St_boot reads them *)

val files : (string * string) list
