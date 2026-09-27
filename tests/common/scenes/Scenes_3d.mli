(* The scenes of the 3D software rasterizer (see Golden_scene.mli):
 * every 3D example's scene, some with debug keys pressed. Not included:
 * TinyMinecraft (slow, and a 1.5 MB frame; see
 * scripts/frames/ref_frames_3d.sh for a manual check), StarCollector3d
 * (random stars). *)

val scenes : Golden_scene.scene list
val scripted : Golden_scene.scripted list
val scripted_flagged : Golden_scene.scripted_flagged list
