(* CATALOG.md checked against the repository. The programs are the
 * executables of the games' genres (games/<genre>/) and of apps/, as
 * their dune files name them; a test per program says it has
 *  - its row, a link to <dir>/<Name>.ml,
 *  - its screenshot, the golden frame tests/2d/golden/<Name>.png
 *    (tests/3d/ for a game whose row says 3D),
 *  - its web page, <dir>/web/<Name>.html,
 * the conventions CATALOG.md's introduction states; and one more test
 * says that every row names a program that still exists. *)
val tests : Testo.t list
