(* Model-View-Controller: the model is the truth, and the views watch
 * it (notes_gui.md section 4).
 *
 * Trygve Reenskaug, at Xerox PARC, December 1979, for Smalltalk-80 --
 * the oldest name in this whole area, and the one most argued about
 * since, because almost nothing called MVC today is what he wrote
 * down. His arrangement:
 *
 *        +---------+   changed!   +-------+
 *        |  model  | -----------> | view  |   the view asks the model
 *        +---------+              +-------+   what to draw, and redraws
 *             ^                       |       itself when told
 *             |  change it            | the mouse, the keys
 *             |                       v
 *        +--------------------------------+
 *        |          controller            |
 *        +--------------------------------+
 *
 * The one idea worth keeping, and the reason it beat callbacks: the
 * count is in *one* place. A view never holds the truth, it shows it;
 * when the model changes it says so, and every view that cares reads
 * it again. The bug of Retained.mli -- a label saying something the
 * program does not believe -- cannot be written, because the label is
 * not where the number lives.
 *
 * What it costs, and what MVU (the playground itself) answers:
 *
 *   - **the notification graph**. Who is observing what is invisible
 *     in the code and only exists at run time. A change that touches
 *     three models wakes every view of each, in an order nobody
 *     chose, and a view woken twice redraws twice;
 *   - **it must be unsubscribed**. An observer outlives the view that
 *     registered it unless somebody remembers to remove it -- the
 *     classic leak of every observer system;
 *   - **the controller is the vaguest box anybody has drawn**. Half
 *     the arguments about MVC since 1979 are about what belongs in
 *     it, which is a sign that it is not a real thing so much as
 *     "the rest".
 *
 * This module is only the model half -- a value, and who to tell when
 * it changes. The views here are Retained widgets that re-read it,
 * and the controller is the callbacks on them, which is exactly
 * Smalltalk's arrangement with OCaml's spelling. *)

type 'model t

val create : 'model -> 'model t
val get : 'model t -> 'model

(* [change t f]: the new model, and then everybody who is watching is
 * told, in the order they signed up *)
val change : 'model t -> ('model -> 'model) -> unit

(* [on_change t f]: [f] is called after every change. There is no way
 * to stop watching, on purpose: a toolkit this size does not need it,
 * and its absence is the leak named above, in one line. *)
val on_change : 'model t -> (unit -> unit) -> unit

(* how many times the observers have been told, which is the number
 * worth looking at when comparing this with the other three *)
val notifications : 'model t -> int
