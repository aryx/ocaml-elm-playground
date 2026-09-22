(* A fighter as a skeleton of boxes: joints with angles, and poses
   interpolated between keyframes.

   This is gamekits/brawler/Stickman.mli in three dimensions, and its .mli
   says so: a character can be a few dozen drawings, or it can be a
   skeleton whose pose is the angles of its joints, and a move a few key
   poses with the frames between filled in. Virtua Fighter (Yu Suzuki,
   Sega AM2, 1993) was the first fighting game to do it in 3D, with
   flat-shaded boxes, and it is still how every 3D character is
   animated.

   What three dimensions add is the *hierarchy*. In 2D a stick figure
   is a handful of lines, and each one can be drawn where it lies. A
   limb here is a box drawn in its parent's frame:

       shoulder o           the forearm is built at the origin, bent at
                 \          the elbow, then moved down the arm's length
        upper arm \         and carried along when the upper arm turns
                   o elbow  -- so the shoulder's angle moves the hand
                    \       without the hand knowing about it
           forearm   \
                      x hand

   which is the whole of "hierarchical transforms": a child is composed
   into its parent's group *before* the parent is rotated, so a joint's
   angle moves everything below it. [draw] is fifteen lines of that, and
   is the point of this module.

   The proportions are Stickman's, so that the same fighter can be drawn
   as sticks or as boxes: the legs half the height, the torso 0.3 of it,
   an arm 0.34, the head the rest.

   Part of the brawler kit (gamekits/brawler/, with Hitbox.mli,
   Frame_data.mli and Stickman.mli), in its own library because it
   draws: the 3D playground is a virtual library, so a 2D game linking
   the brawler kit would otherwise have to link a 3D backend too. The
   rules of a fight -- the frames of a move, the boxes it hits with --
   are the same in both, and stay next door. *)

open Playground
open Playground3d

(* {1 Poses} *)

(* A limb: its upper part's two angles at the shoulder or hip, and how
 * far the elbow or knee is bent.
 *
 * [pitch] is from straight down, positive forward (the way the figure
 * faces), so a straight arm at a fighter's side is 0 and a punch
 * straight out is 90. [yaw] swings the limb away from the body, which
 * is what a hook punch or a round kick needs and what a stick figure
 * cannot show. [bend] closes the joint, always the same way a knee or
 * an elbow closes: positive. *)
type limb = { pitch : number; yaw : number; bend : number }

(* the whole figure: the torso's lean (from upright, positive forward)
 * and twist (positive towards the way it faces), and its four limbs *)
type pose = {
  lean : number;
  turn : number;
  front_arm : limb;
  back_arm : limb;
  front_leg : limb;
  back_leg : limb;
}

(* standing, relaxed: everything straight down, a little bend in the
 * knees *)
val stand : pose

(* a limb, shorter to write than the record *)
val limb : ?yaw:number -> ?bend:number -> number -> limb

(* {1 Animating} *)

(* [lerp a b t]: each angle of [a] moved [t] of the way (0. to 1.) to
 * [b]'s *)
val lerp : pose -> pose -> number -> pose

(* [at keys frame]: the pose at [frame], [keys] being the key poses at
 * their frames, in order: between two, interpolated; before the first,
 * the first; after the last, the last. A move is written as three or
 * four of these and the sixty frames a second fill themselves in. *)
val at : (int * pose) list -> int -> pose

(* {1 Drawing} *)

(* [draw ?front_hand ?back_hand ~body ~back ~skin height heading pose]: the figure [height]
 * tall standing straight, its feet on y = 0, built at the origin and
 * turned to [heading] -- degrees, the playground's own (0 faces -z, 90
 * faces +x, as Camera3d and every 3D game here use). [back] darkens
 * the far arm and leg so the figure reads at a glance, as Stickman's
 * does; [skin] is the head, and the nose that says which way it looks.
 *
 * The lowest foot is not planted: a pose with a raised knee stands on
 * the other foot, and the caller places the figure.
 *
 * [front_hand] and [back_hand] are what the hands hold, a sword, a
 * shield: built with the fist at the origin and the forearm going on
 * down -y, as the arm hangs at pitch 0. The thing becomes one more
 * level of the hierarchy, carried by the elbow and the shoulder, so a
 * sword held that way points wherever the forearm does -- a blade of
 * length l is [box c w l d |> move_y3d (-. l /. 2.)] -- and a swing is
 * the arm's keyframes, nothing more (TinyZeldaOcarina's). *)
val draw :
  ?front_hand:shape3d ->
  ?back_hand:shape3d ->
  body:color -> back:color -> skin:color -> number -> number -> pose -> shape3d

(* [hand height heading pose]: where the front fist ends up, for a
 * figure of [height] turned to [heading] -- the same hierarchy
 * followed by arithmetic instead of boxes. A game can hang a spark on
 * it, or ask what it reaches. A pitch of 90 is a straight punch, so
 * the hand is then an arm's length in front of the shoulder. *)
val hand : number -> number -> pose -> number * number * number

(* [foot height heading pose]: the same for the front foot *)
val foot : number -> number -> pose -> number * number * number
