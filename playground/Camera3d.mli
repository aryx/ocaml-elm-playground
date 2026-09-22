(* Cameras for 3D games: where to put the eye, given what it follows.

   Playground3d's [camera] is just an eye and a target. Games then keep
   writing the same few lines: behind the car, a bit above, looking a bit
   ahead; from the rider's eyes; from above; turning around the title's
   scene. This module is those lines, named, as Camera2d is for 2D games
   (and a layer on top of Playground3d the same way: only its [camera]
   and shapes).

       from_far          orbit
          \                .-.
           \   behind     (   )  around the scene, for a title
            \    /         `-'
             \  /
              o-> ------> target: [ahead] of the thing, [look] up
            thing, [back] behind it, [height] above: [behind]

   The thing a camera follows is a [pose]: where it is, and which way it
   faces on the ground (its heading, as in games3d/TinyVirtuaRacing.ml,
   TinyBattlezone.ml: 0 towards -z, 90 towards +x).

   A camera rigidly attached to what it follows jerks with it: a Tron
   cycle turning 90 degrees in one frame swings the whole world at once.
   [follow] smooths it: the camera, kept in the model, moves a fraction
   of the way towards where it should be each frame (Camera2d.follow's
   lerp-smoothing, for the eye and the target).

   The camera problem is 3D games' own: Super Mario 64 (1996) made its
   camera a character, Lakitu filming Mario, whose view you could
   change (and blame); John Nesky's "50 Game Camera Mistakes" (GDC 2014)
   lists what goes wrong. Virtua Racing (1992) had four views on four
   buttons (games3d/TinyVirtuaRacing.ml's v key), and so does
   games3d/TinyTron3d.ml. Not here yet (exercises): a camera avoiding
   walls between it and the player (a ray cast back from the player),
   and a camera the mouse turns around the player (orbit with mdx).

   Also [floor] and [sky]: the ground, a sky and a horizon for the views
   that see far, lit well by flat shading whatever the heading (see
   [sky]'s comment).
*)

open Playground
open Playground3d

(* what a camera follows: a position, and a heading on the ground, in
 * degrees: 0 towards -z, 90 towards +x (clockwise, seen from above) *)
type pose = { x : number; y : number; z : number; heading : number }

(* the heading's direction on the ground, (x, z): forward 0 = (0, -1),
 * forward 90 = (1, 0) *)
val forward : number -> number * number

(* {1 Following a pose} *)

(* [behind ~back ~height ~ahead ~look pose]: the eye [back] behind the
 * pose and [height] above it, looking at the point [ahead] of it and
 * [look] above it. A negative [back] puts the eye in front of the
 * pose's center: in its cockpit. E.g. back 9, height 3.5, ahead 8, look
 * 1: a racing game's chase view. *)
val behind : ?fov:number -> back:number -> height:number -> ahead:number -> look:number -> pose -> camera

(* [chase pose]: [behind] with back 7, height 3.5, ahead 6, look 0.5 *)
val chase : pose -> camera

(* [cockpit pose]: the rider's eyes: [behind] with back -0.3, height
 * 0.9, ahead 10, look 0.7 *)
val cockpit : pose -> camera

(* {1 Looking at a place} *)

(* [looking_down ~height (x, y, z)]: straight down on (x, y, z) from
 * [height] above (almost straight: a camera needs a direction for its
 * "up", so the eye is 0.01 south of it) *)
val looking_down : ?fov:number -> height:number -> number * number * number -> camera

(* [from_far ~offset center]: looking at [center] from [center + offset];
 * with a far offset and a narrow field of view (e.g. (-300, 300, 300)
 * and 13 degrees), the perspective barely shows: nearly isometric, the
 * look of Zaxxon and Marble Madness *)
val from_far : ?fov:number -> offset:number * number * number -> number * number * number -> camera

(* [orbit ~distance ~height ~look angle center]: on a circle of radius
 * [distance] around [center], [height] above it, at [angle] degrees
 * (0: south of it, +z), looking at [look] above the center: a title's
 * turning view, with [angle] from Playground.spin *)
val orbit : ?fov:number -> distance:number -> height:number -> look:number -> number -> number * number * number -> camera

(* {1 Smoothing} *)

(* [follow fraction wanted cam]: [cam]'s eye and target moved [fraction]
 * of the way to [wanted]'s (its other settings taken from [wanted]):
 * called every frame with the camera kept in the model, it swings
 * smoothly after a turn, and glides from one view to another. With
 * 0.2, about 90% of the way in 10 frames (1 - 0.8^10 = 0.89). *)
val follow : number -> camera -> camera -> camera

(* [orthographic ~height cam]: [cam] with its perspective taken away.
 * [height] of the world fits up the screen -- at every depth, which is
 * the whole point: two equal things are drawn equal however far apart
 * they are.
 *
 * The words around this are worth untangling once, because games use
 * them loosely:
 *
 *   projection
 *     |
 *     +- perspective ....... divide x and y by the depth. Things
 *     |                      shrink with distance, parallel rails
 *     |                      meet. Photographs; first-person games.
 *     |
 *     +- parallel .......... do not divide. Nothing shrinks, parallel
 *          |                 stays parallel. Plans and blueprints;
 *          |                 strategy, puzzle and isometric games.
 *          |
 *          +- orthographic .. the rays are square to the picture
 *               |             (this function)
 *               |
 *               +- axonometric .. and the *world* is turned first, so
 *                    |            that three faces of a cube show at
 *                    |            once
 *                    |
 *                    +- isometric: turned so the three axes are
 *                    |             equally foreshortened -- the view
 *                    |             direction (1, 1, 1), the axes 120
 *                    |             degrees apart on the screen
 *                    |
 *                    +- dimetric, trimetric: two of them equal, or
 *                                  none. Most games called isometric
 *                                  are really one of these: they pick
 *                                  a 2:1 diamond because it draws
 *                                  without seams on a pixel grid.
 *
 * So: this function gives you the *parallel* half; pointing the camera
 * along (1, 1, 1) gives you the isometric part. Together they are what
 * gamekits/isometric does by hand on the 2D playground, two lines of
 * arithmetic and a sort.
 *
 * What it costs, and what that buys:
 *
 *   the view direction (1, 1, 1) sends these two world points
 *   to the same pixel, and every pair like them:
 *
 *       (0, 0, 0)          .  <- both drawn here
 *       (3, 3, 3)
 *
 *   - an isometric game must therefore draw a *shadow* under anything
 *     off the ground, or the player cannot tell how high it is
 *     (games2.5d/TinyZaxxon.ml is built around that shadow);
 *   - and a game can instead take the ambiguity as its material:
 *     games3d/TinyMonumentValley.ml joins a near stair to a far one
 *     because, from here, they touch.
 *
 * The depth still decides what is in front of what -- the z-buffer
 * works exactly as before. Only the divide by it is gone. *)
val orthographic : height:number -> camera -> camera

(* {1 The world around} *)

(* [floor ?color ?ground cam]: a floor at the height [ground],
 * following the camera, 800 around the eye: far enough for the horizon,
 * near enough to stay within the camera's far plane (a face reaching
 * past it is dropped) *)
val floor : ?color:color -> ?ground:number -> camera -> shape3d

(* [sky ?sky ?horizon ?ground cam]: for the views that see far: a [sky]
 * above the eye, and a [horizon] skirt far ahead, rising from the
 * [ground] to just above the eye (the land, or sea, going on to the
 * horizon: give it the floor's color), both following the camera.
 *
 * Flat shading lights a face by the way it faces the sun (up and to the
 * side): a vertical sky ahead would be dark at some headings. The sky is
 * therefore a plane turned *up*, above the eye, seen from below -- which
 * needs the back faces drawn: the game runs with [backface_culling =
 * false] (see Playground3d.rendering). For the same reason the horizon
 * is a slope facing up, not a wall. *)
val sky : ?sky:color -> ?horizon:color -> ?ground:number -> camera -> shape3d list
