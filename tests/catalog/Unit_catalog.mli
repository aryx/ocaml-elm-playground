(* CATALOG.md checked against the repository. The programs are the
 * executables of games/ (and its genres' directories), games2.5d/,
 * games3d/ and apps/, as their dune files name them; a test per program
 * says it has
 *  - its row, a link to <dir>/<Name>.ml,
 *  - its screenshot, the golden frame tests/2d/golden/<Name>.png
 *    (tests/3d/ for games3d/, and for a genre's game whose row says
 *    3D),
 *  - its web page, <dir>/web/<Name>.html (games3d/webgl/; a genre's
 *    3D games are in its web/ too, on WebGL),
 * the conventions CATALOG.md's introduction states; and one more test
 * says that every row names a program that still exists. *)
val tests : Testo.t list
