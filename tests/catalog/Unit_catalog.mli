(* CATALOG.md checked against the repository. The programs are the
 * executables of games/, games2.5d/, games3d/ and apps/, as their dune
 * files name them; a test per program says it has
 *  - its row, a link to <dir>/<Name>.ml,
 *  - its screenshot, the golden frame tests/2d/golden/<Name>.png
 *    (tests/3d/ for games3d/),
 *  - its web page, <dir>/web/<Name>.html (games3d/webgl/),
 * the conventions CATALOG.md's introduction states; and one more test
 * says that every row names a program that still exists. *)
val tests : Testo.t list
