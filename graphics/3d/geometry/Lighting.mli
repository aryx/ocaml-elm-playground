(* How bright is a surface, given the direction it faces? The one
 * lighting formula of the 3D Playground, used by all its backends (the
 * software rasterizer, the web one's flat shading, and the OpenGL one,
 * whose fragment shader repeats it in GLSL).
 *
 * Lambert's cosine law: a surface facing the light gets all of it; tilted
 * by an angle a, the same light spreads over 1/cos(a) times more surface,
 * so each point gets cos(a) of it; facing away, none:
 *
 *     normal = light     normal   light      normal
 *          ^               ^ a  /            ^
 *          |               |   /             |
 *          |               |  /              |
 *          |               | /               |
 *   -------+-------  ------+-------    ------+-------
 *                                             \
 *                                              \ light (from below)
 *      a = 0: 1           cos a        a > 90 degrees: max(0, cos a) = 0
 *
 * and cos(a) is the dot product of the two unit vectors, the normal and
 * the direction towards the light. Plus a floor, the [ambient] light
 * bouncing around in a real scene, so no face is pitch black:
 *
 *   brightness = ambient + (1 - ambient) * max(0, normal . light_dir)
 *
 * With ambient = 0.25: a face turned to the light is at 1.0, one at 60
 * degrees from it at 0.25 + 0.75 * 0.5 = 0.625, one at 90 degrees or
 * more at 0.25.
 *
 * Reference: Johann Heinrich Lambert, "Photometria", 1760. *)

(* The direction towards the "sun", a unit vector *)
val light_dir : Vec3.t

(* The brightness of faces facing away from the light, 0.25 *)
val ambient : float

(* [brightness_of_normal n], for a unit normal [n]: from [ambient] to 1 *)
val brightness_of_normal : Vec3.t -> float
