(* The golden frames of the 2D software rasterizer (see
 * tests/common/Testutil_golden.mli): examples and games, some with
 * debug keys pressed. Not included: those that download their images
 * (examples/Turtle, examples/Mario: tests shouldn't need the network),
 * those random from run to run (games/Snake, games/Tetris:
 * Random.self_init), and the Cairo backend (its pixels depend on the
 * installed Cairo). *)
val tests : Testo.t list
