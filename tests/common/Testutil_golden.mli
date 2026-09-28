(* Golden frame tests: one frame of an example or game, compared pixel
 * by pixel with a committed PNG, the "golden" frame. Used by
 * tests/2d/ (the 2D software rasterizer) and tests/3d/ (the 3D one).
 *
 * Each scene's executable runs with its clock frozen (-fixed-time), some
 * debug keys pressed before its first frame (-keys), and frame n dumped
 * as a PPM (-dump-frame n; see Native_loop_2d and playground3d's
 * Native_loop_3d), under SDL's "dummy" video driver: the window gets an
 * in-memory surface, so no display is needed, and the pixels are the
 * same as on a real window.
 *
 * When a frame differs, its test fails saying how many pixels differ
 * and where, and writes the new frame to <dir>/actual/<scene>.png (in
 * _build/default/). If the change is intended (look at it!), the
 * Makefile's approve target makes the new frames the golden ones.
 *
 * The committed frames were generated on an arm64 Linux machine: on
 * another one, expect a few frames to differ slightly without anything
 * having changed (rounding in the antialiasing, in the shading or in a
 * z-comparison landing the other way), a handful of pixels off by 1 in
 * 2D and, where a whole span of a triangle tips over, more in 3D. A
 * scene deep into a simulation drifts much further than that -- the
 * 300 marbles of PhysicsMarbles differ in 10000 pixels after two
 * seconds, because one rounding apart is enough for a pile to settle
 * differently. That is not a regression to chase; a real one moves
 * pixels you can see in <dir>/actual/. *)

(* The scenes, as Golden_scene.mli describes them (data of their own,
 * tests/common/scenes/, which tinybox's menu reads too) *)
type scene = Golden_scene.scene
type scripted = Golden_scene.scripted
type flagged = Golden_scene.flagged
type scripted_flagged = Golden_scene.scripted_flagged

(* Scenes deep into a game (more than 100 frames) cost seconds of CPU
   each, and they all run at once: those are skipped unless the
   environment variable GOLDEN is "all". "make test" then keeps every
   example's frames and the cheap first frame of each game -- enough to
   catch a rendering regression -- and "make test-golden-all" runs the
   gameplay ones too (do that before a release, or after touching a
   renderer). GOLDEN=none skips every scene: "make test-lite", for a
   change that only moves or renames things, where the build is the
   check that matters. *)

(* [tests ~dir ~approve ?scripted ?flagged ?scripted_flagged scenes]: a
 * test per scene and per scripted, flagged, and scripted and flagged
 * scene, for a test running in _build/default/<dir>, with its golden
 * frames in <dir>/golden/ and its Makefile target [approve] (named in
 * the failure messages) *)
val tests :
  dir:string ->
  approve:string ->
  ?scripted:scripted list ->
  ?flagged:flagged list ->
  ?scripted_flagged:scripted_flagged list ->
  scene list ->
  Testo.t list
