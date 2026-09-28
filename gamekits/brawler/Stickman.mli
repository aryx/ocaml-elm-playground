(* Stickman: fighters drawn as stick figures, in poses.

   A fighting game's character is a few dozen pictures -- standing,
   walking, punching, falling -- drawn by artists frame by frame
   (Street Fighter II's Ryu has hundreds). Without artists, a character
   can be a skeleton: a torso, a head, two arms and two legs of two
   segments each, and a pose is the angles of its joints. A move is then
   a few key poses at given frames, and the frames between are
   interpolated ([at]): keyframe animation, the way 3D characters are
   animated (and Virtua Fighter's, see plan_games3d.md section 11).

          o        the angles: a limb's from straight down, positive
         /|\__     forward (the way the figure faces); the torso's lean
          |        from straight up, positive forward. A punch: the
         / \       front arm at (90, 90), straight out.

   Stick figures fight well: Stick Fight, and the flash games of the
   2000s (Xiao Xiao) were only that.

   Part of the brawler kit (gamekits/brawler/), with Hitbox.mli and
   Frame_data.mli. *)

open Playground

(* a limb's two angles: its upper part's (arm, thigh) and its lower
 * part's (forearm, shin), each from straight down, in degrees *)
type limb = number * number

type pose = { lean : number; front_arm : limb; back_arm : limb; front_leg : limb; back_leg : limb }

(* standing, relaxed *)
val stand : pose

(* [lerp a b t]: each angle of [a] moved [t] of the way (0. to 1.) to
 * [b]'s *)
val lerp : pose -> pose -> number -> pose

(* [at keys frame]: the pose at [frame], [keys] the key poses at their
 * frames (in order): between two, interpolated. E.g. [(0, a); (10, b)]
 * at frame 5: [lerp a b 0.5]; before the first, the first; after the
 * last, the last. *)
val at : (int * pose) list -> int -> pose

(* [hand h p]: where the front hand is, for a figure [h] high facing
 * right, its lowest foot on the ground at (0, 0). E.g. 200 high,
 * standing straight (legs at 0), the front arm at (90, 90): (68, 160),
 * the arm's 0.34 of the height, forward, at the shoulder's height (the
 * legs 0.5, the torso 0.3). *)
val hand : number -> pose -> number * number

(* [draw color back h facing p]: the figure, [h] high (standing
 * straight, from the feet to the top of the head), facing right (1.) or
 * left (-1.), its lowest foot at (0, 0); its back arm and leg in [back],
 * a darker color, behind *)
val draw : color -> color -> number -> number -> pose -> shape
