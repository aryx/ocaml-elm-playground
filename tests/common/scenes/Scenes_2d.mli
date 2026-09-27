(* The scenes of the 2D software rasterizer (see Golden_scene.mli):
 * examples and games, some with debug keys pressed. Not included: those
 * that download their images (examples/Turtle, examples/Mario: tests
 * shouldn't need the network), those random from run to run (Snake,
 * Tetris: Random.self_init), and the Cairo backend (its pixels depend
 * on the installed Cairo). *)

val scenes : Golden_scene.scene list
val scripted : Golden_scene.scripted list
val flagged : Golden_scene.flagged list
val scripted_flagged : Golden_scene.scripted_flagged list
