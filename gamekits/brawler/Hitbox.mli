(* Hitbox: the boxes that hit, and the boxes that get hit.

   A punch lands when its fist touches the other's body, but games don't
   test fists against bodies: they test boxes. A fighter has a hurtbox
   (where it can be hit: its body, a box, or a few), and a move, while
   it's active, a hitbox (where it hits: around the fist, the foot, the
   fireball); a hit is a hitbox overlapping a hurtbox. Designers draw
   them frame by frame, and fighting game players study them: a move
   whose hitbox reaches farther than its own hurtbox wins trades
   ("priority"), and Street Fighter II's boxes are the reason some kicks
   beat everything.

          hurtbox   hitbox, while the punch is active
         +------+  +--+
         |  o   |--|  |      facing left, the hitbox is mirrored:
         | /|\  |  +--+      its x relative to the fighter, negated
         | / \  |
         +------+

   A box is its center and its size, relative to the fighter's feet,
   given for a fighter facing right (x forward, y up); [place] puts it in
   the world for a fighter facing either way.

   Part of the brawler kit (gamekits/brawler/), with Frame_data.mli and
   Stickman.mli; used by games/TinyFinalFight and games/TinyStreetFighter. *)

open Playground

type box = { x : number; y : number; w : number; h : number }

(* [place facing (fx, fy) b]: [b] of a fighter with its feet at (fx,
 * fy), facing right (1.) or left (-1.). E.g. a punch's box at (40, 100),
 * 30 x 20, of a fighter at (200, 0) facing left: at (160, 100). *)
val place : number -> number * number -> box -> box

(* [overlap a b]: the two boxes (placed) overlap; touching isn't
 * overlapping. E.g. the punch above against a body at (130, 80), 40 x
 * 120: yes (30 apart in x, less than (30 + 40) / 2). *)
val overlap : box -> box -> bool

(* a box, translucent, to see them (a game's hitboxes flag) *)
val draw : color -> box -> shape
