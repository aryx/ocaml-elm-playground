(* The golden frames of the 3D software rasterizer (see
 * tests/common/Testutil_golden.mli): every examples3d scene, some with
 * debug keys pressed. Not included: games3d/Minecraft3d (slow, and a
 * 1.5 MB frame; see scripts/frames/ref_frames_3d.sh for a manual check),
 * games3d/StarCollector3d (random stars). *)
val tests : Testo.t list
