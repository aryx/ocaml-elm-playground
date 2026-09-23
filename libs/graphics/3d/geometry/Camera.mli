(* A camera: where it is, where it looks, how wide it sees -- and the
 * first two steps of every 3D renderer, putting the scene in front of
 * the camera and then flattening it:
 *
 *   world coordinates        view coordinates           normalized device
 *   (the scene's)       ->   (the camera's: x right, -> coordinates (x, y
 *                            y up, z forward)           in -1..1 on screen)
 *        [view]                     [ndc]
 *
 * The view step is a change of basis: the camera's own three directions,
 *
 *                 up                     forward = from eye to target
 *                 ^   ^ forward          right   = forward x (0, 1, 0)
 *                 |  /                   up      = right x forward
 *                 | /
 *          eye    +----> right
 *
 * and a point's view coordinates are how far it is along each of them
 * from the eye: three dot products (see notes_3d.md, section 4).
 *
 * The ndc step is the perspective: things twice as far look half as
 * big, i.e. divide by the depth z. The field of view [fov] (vertical, in
 * degrees) sets the scale: f = 1 / tan(fov / 2), so a point at the top
 * edge of the view (y / z = tan(fov / 2)) lands at ndc y = 1. Example:
 * with fov = 90 degrees, f = 1, and a point 10 in front of the camera
 * and 5 up is at ndc y = 5 / 10 = 0.5, half way to the top.
 *
 * Reference: Lawrence G. Roberts, "Machine Perception of
 * Three-Dimensional Solids", MIT PhD thesis, 1963 (perspective
 * projection, and homogeneous coordinates, for computer graphics). *)

type t = {
  eye : Vec3.t;
  target : Vec3.t;
  (* claude: which way is up on the screen, a hint: (0, 1, 0), the
   * world's up, for a camera that doesn't roll; tilted, the picture
   * turns the other way (a plane banking, Descent's ship). Only its
   * part across [forward] counts (see [basis]). *)
  up : Vec3.t;
  (* vertical field of view, in degrees; not used when [ortho] is set *)
  fov : float;
  (* 0 for a perspective camera, which is the usual one; otherwise the
   * height of the view in world units, and the camera is
   * *orthographic*: it does not divide by the depth at all.
   *
   * The two kinds of camera differ in one thing only, where the rays
   * that reach the picture come from:
   *
   *    perspective: they meet at the eye      orthographic: they are
   *                                           parallel -- no eye, only
   *                                           a direction
   *
   *      far      near                          far      near
   *     +----+   +--+                          +----+   +----+
   *      \    \   |  |                          |    |   |    |
   *       \    \  |  |                          |    |   |    |
   *        \    \ |  |                          |    |   |    |
   *         +----+ +--+---> o  the eye           +----+   +----+
   *                                                |        |
   *     the far wall is drawn smaller           both walls are drawn
   *     (x and y divided by the depth z)        exactly the same size
   *
   * So with [ortho = 10.], ten units of world fit up the screen at
   * *every* depth: a thing 5 up is half way to the top whether it is
   * 10 away or 90. With [fov = 90.] instead, 5 up is half way at a
   * depth of 10 and a tenth of the way at 90.
   *
   * The depth is still computed and still used -- for the near and far
   * planes, and for the z-buffer deciding what is in front -- it is
   * only the *divide* that is gone.
   *
   * What it is for: plans and blueprints, strategy and puzzle games,
   * and above all the isometric view, which is this projection from a
   * particular direction (see Playground3d's Camera3d.orthographic for
   * the family, and gamekits/isometric, which does the same arithmetic by
   * hand on the 2D playground). It has one famous consequence: with
   * the view direction (1, 1, 1), the points (0, 0, 0) and (3, 3, 3)
   * land on the same pixel, and nothing in the picture can tell them
   * apart. An isometric game therefore draws a shadow to say how high
   * a thing is -- and TinyMonumentValley builds its impossible
   * staircases out of exactly that ambiguity. *)
  ortho : float;
  (* only what's between these two depths is drawn *)
  near : float;
  far : float;
}

(* The camera's (right, up, forward) unit vectors, "up" being as close
 * to [up] (default the world's (0, 1, 0)) as possible (a camera looking
 * along [up] has no right: out of scope) *)
val basis : ?up:Vec3.t -> eye:Vec3.t -> target:Vec3.t -> unit -> Vec3.t * Vec3.t * Vec3.t

(* [view camera point]: [point] in view coordinates, (along right,
 * along up, along forward = depth) *)
val view : t -> Vec3.t -> Vec3.t

(* f = 1 / tan(fov / 2), the scale of the perspective (of no use to an
 * orthographic camera, which has none) *)
val focal : t -> float

(* [ndc camera ~aspect (x, y, z)]: a point already in view coordinates,
 * divided by its depth (or, for an orthographic camera, not divided at
 * all) to normalized device coordinates, x and y in
 * -1..1 for what's in view; [aspect] is the screen's width / height (x
 * is squeezed by it, so a square stays square). None when its depth z
 * is not between [near] (included: where Clip puts the points it
 * creates) and [far] (excluded). *)
val ndc : t -> aspect:float -> Vec3.t -> (float * float) option
