(* Transition: a layout's rectangles moving to another layout, each
   known by its key (a treemap's paths, a list's items): Core
   Animation's implicit animation of a layer's frame, for a whole
   layout at once, and what Keynote calls Magic Move.

      before                         after
     +------+-----------+          +-----------------------+
     | a    | b         |          | b                     |
     +------+-----------+   ==>    |                       |
     | c                |          +-----------+-----------+
     +------------------+          | c         | d (new)   |
                                   +-----------+-----------+

   A key in both moves from its old rectangle to its new one; a key only
   in the new layout (d) enters, grown from a point (its [enter] origin,
   by default its own centre) and faded in; a key only in the old (a)
   leaves, shrunk to its [leave] point and faded out. One progress, from
   0 to 1 (Animation's, through its timing curve), drives them all: a
   frame is [at t p].

   Worked example (the tests'): b from (50, 0, 50x50) to (0, 0,
   100x50), at 0.5: (25, 0, 75x50); d entering at (50, 50, 50x50) from
   its centre, at 0.5: (62.5, 62.5, 25x25), half faded. *)

type rect = { x : float; y : float; w : float; h : float }

(* a rectangle in the frame, and how opaque, 0 to 1 *)
type 'k frame = { key : 'k; rect : rect; alpha : float }

type 'k t

val lerp_rect : rect -> rect -> float -> rect

(* [make ?enter ?leave ~before ~after ()]: [enter k r] the rectangle a
   new key grows from (its [r] shrunk to its centre if not given);
   [leave k r] the one a gone key shrinks to *)
val make : ?enter:('k -> rect -> rect) -> ?leave:('k -> rect -> rect) -> before:('k * rect) list -> after:('k * rect) list -> unit -> 'k t

(* the frame at a progress in [0, 1]: the moving and entering keys in
   [after]'s order, then the leaving ones *)
val at : 'k t -> float -> 'k frame list
