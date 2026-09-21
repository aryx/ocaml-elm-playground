(* An isometric view: a world seen from one fixed angle, on the flat
   playground.

   Three world axes, two screen ones. [across] says where one unit of
   world x goes on the screen, [along] where one unit of z goes, and
   [up] how many pixels one unit of height is worth, straight up:

     sx = across_x * x + along_x * z
     sy = across_y * x + along_y * z + up * y

   That is the whole of it. There is no camera, no perspective and no
   depth per pixel: a thing twice as far away is drawn exactly as big,
   which is why the shapes of such a world never distort, and why the
   arithmetic is two multiplications and an add. Zaxxon (Sega, 1982)
   was the first game to be seen this way; Q*bert (1982), Marble
   Madness (1984), Knight Lore (1984) and its Filmation engine,
   Populous (1989), Syndicate (1993) and Diablo (1996) are all this
   projection with different things standing on it.

   ("Isometric" is the word everyone uses. The pedantic one, and Sega's
   own in 1982, is *axonometric*: isometric is strictly the case where
   the three axes are equally foreshortened, which a game's view
   usually is not -- Zaxxon draws x longer than z so its fortress reads
   as a corridor, and a tile game usually wants the 2:1 diamond that
   pixels can draw without seams.)

        y
        |                    far          the two screen vectors:
        |                     .           [across] and [along], with
        |                  .     .        height straight up. A world
        +----- x        .     +     .     point is a sum of the three
       /                   .     .
      z                       .
                            near

   {1 The two problems this projection makes, and their answers}

   - **Height is invisible.** Two pictures a hundred pixels apart may
     be a thing at altitude 100 and a thing on the ground, or two
     things at the same height a hundred units apart along z: the
     projection throws away exactly the number a player needs. Hence
     [shadow]: draw the thing twice, once where it is and once at
     y = 0 directly below it, and the vertical gap between the two
     *is* the altitude, in pixels, to be read off the screen. Every
     isometric game with anything off the ground does this.

   - **What hides what.** With no depth per pixel, the order of the
     drawing is the whole depth test: [sorted] puts the far things
     first. [depth] is what it sorts on -- the distance along the view
     direction, not z alone, since moving across the world moves you
     towards the eye as well. The order is exact as long as no two
     things interleave (one thing per place in the world, nothing
     leaning over anything else), which is the same restriction the
     rest of games2.5d/ lives by.

   {1 What stands between a thing and the eye}

   [sorted] gives every two shapes an order, but one key per object is
   only an approximation of it: a wall is a whole face, and a player
   can be in front of one end of it and behind the other. For the one
   pair that has to be right -- the player against the walls -- ask
   the line of sight instead.

   [toward_eye] is the one direction this projection flattens to
   nothing: walk along it and you stay on the same pixel, and because
   there is no perspective it is one vector for the whole world rather
   than a ray per point. [sight] walks it from a point to a given
   plane and says where it comes out, and the game asks whether that
   point is inside one of its walls. Two uses, and a game picks one:
   draw the walls that hide the player *after* him and the rest
   before, which is exact; or draw him again over them, faintly, so
   that he is never lost behind one (Knight Lore, 1984, and most
   isometric games with a roof). The first is what
   games2.5d/TinyZaxxon does: being hidden by what is in front of you
   is the view telling the truth, and worth keeping.

   Related work: the tile-based isometric renderers of the 1990s
   (SimCity 2000, Age of Empires, Diablo), which all sort by the tile
   grid's (row + column); Clint Bellanger's "Isometric Tiles Math"
   (2011, the article most isometric games' code comes from); Q*bert's
   pyramid, which needs no sort at all because it is drawn top row
   first. *)

open Playground

type t

(* [make ~across ~along ~up]: the view whose world x axis goes [across]
 * on the screen, whose z axis goes [along] it, and whose height is
 * [up] pixels per unit (1. unless the game scales it). E.g. Zaxxon's
 *   make ~across:(0.85, -0.30) ~along:(0.34, 0.42) ~up:1.
 * and a 2:1 tile grid of 64 by 32 pixels
 *   make ~across:(32., -16.) ~along:(-32., -16.) ~up:1. *)
val make : across:number * number -> along:number * number -> up:number -> t

(* [origin ox oy v]: [v] with the world's origin drawn at (ox, oy) on
 * the screen rather than at its centre *)
val origin : number -> number -> t -> t

(* [follow x z v]: [v] scrolled so that the world point (x, _, z) is
 * the one at the origin -- the whole of scrolling in this projection,
 * a subtraction before the two lines *)
val follow : number -> number -> t -> t

(* {1 Drawing} *)

(* [project v (x, y, z)]: where that world point lands on the screen *)
val project : t -> number * number * number -> number * number

(* [at v p shape]: [shape] put where the world point [p] is *)
val at : t -> number * number * number -> shape -> shape

(* [shadow v p shape]: [shape] put on the ground directly below [p]
 * (its y taken to be 0). Drawn under the thing itself, the gap
 * between them is its height. *)
val shadow : t -> number * number * number -> shape -> shape

(* [ground v (sx, sy)]: the floor point (y = 0) under a screen point,
 * as (x, z): the two lines run backwards. They can be, and with one
 * 2x2 determinant, only because there is no perspective to divide by
 * -- which is how an isometric game turns a mouse click into a place
 * in the world, and why Diablo could be played with one button. *)
val ground : t -> number * number -> number * number

(* [depth v p]: how far [p] is from the eye, along the view direction.
 * Bigger is farther. *)
val depth : t -> number * number * number -> number

(* [sorted l]: the shapes of [l], each with the depth to draw it at,
 * farthest first -- the painter's algorithm, which is exact here as
 * long as nothing interleaves *)
val sorted : (number * shape) list -> shape list

(* {1 The line of sight} *)

(* [toward_eye v]: the direction, in world units, that this projection
 * flattens to nothing: walk along it from any point and you stay on
 * the same pixel. It is a direction and not a ray to a point, since
 * there is no perspective. *)
val toward_eye : t -> number * number * number

(* [sight v p z]: where the line of sight from [p] towards the eye
 * crosses the plane at that z, as (x, y) -- None when the plane is
 * behind [p] (nothing there can hide it). A game asks whether the
 * point that comes back is inside one of its walls. *)
val sight : t -> number * number * number -> number -> (number * number) option
