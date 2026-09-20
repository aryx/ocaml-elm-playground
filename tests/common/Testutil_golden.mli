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
 * 2D and, where a whole span of a triangle tips over, more in 3D.
 * That is not a regression to chase; a real one moves pixels you can
 * see in <dir>/actual/. *)

(* A scene: an executable, from the project's root and without its .exe
 * (e.g. "examples3d/Cubes3d"), the debug keys to press ("" for none),
 * and the frame to compare. Its golden frame is golden/<name>.png, with
 * <name> the executable's basename, plus "_" and the keys if any (e.g.
 * golden/Cubes3d_bf.png). *)
type scene = string * string * int

(* A scene played with game keys: an executable, a label, the frame to
 * compare, and the -script giving the keys held over the frames (see
 * playground/native_common/Input_script.mli), e.g. ("games/software/
 * TinyMario", "jump", 60, "right:1-60,up:20-25"). Its golden frame is
 * golden/<basename>_<label>.png (e.g. golden/TinyMario_jump.png). *)
type scripted = string * string * int * string

(* Scenes deep into a game (more than 100 frames) cost seconds of CPU
   each, and they all run at once: those are skipped unless the
   environment variable GOLDEN_ALL is set. "make test" then keeps every
   example's frames and the cheap first frame of each game -- enough to
   catch a rendering regression -- and "make test-golden-all" runs the
   gameplay ones too (do that before a release, or after touching a
   renderer). *)

(* [tests ~dir ~approve ?scripted scenes]: a test per scene and per
 * scripted scene, for a test running in _build/default/<dir>, with its
 * golden frames in <dir>/golden/ and its Makefile target [approve]
 * (named in the failure messages) *)
val tests : dir:string -> approve:string -> ?scripted:scripted list -> scene list -> Testo.t list
