(* Animation: a value on its way from one place to another, and
   implicit animations, Core Animation's two ideas.

   An animation is only numbers: where from, where to, when it started,
   how long, along which timing curve (Timing). Its value is a formula
   of the time now (Elm's way, libs/juice/Tween.mli's): nothing to step,
   nothing to stop, a replay shows the same frame at the same time.

      value
        to |               .-------   held once done
           |           .-'
           |        .-'     along its timing curve
      from |-------'
           +-------+-------+-------> now
                 start   start + duration

   Implicit animations (Core Animation's layers, 2007): a program only
   says where a thing should be, its target; when the target changes,
   the thing does not jump, it goes there, from wherever it is now --
   its presentation value, perhaps already moving: an animation
   interrupted starts the next from where it was, so there is no jump
   either. A [keyed] holds, for each key (a layer, a rectangle's name),
   its animation; [set] a new target, [value] where it is now.

   Values are anything with a [lerp] (numbers, points, colours,
   rectangles: Transition). *)

type 'a t = { from : 'a; to_ : 'a; start : float; duration : float; timing : Timing.t }

(* [make ?timing ~duration ~now from to_]: starting now; [timing]
   Timing.default *)
val make : ?timing:Timing.t -> duration:float -> now:float -> 'a -> 'a -> 'a t

(* a value already there, not moving *)
val still : 'a -> 'a t

(* the way gone at [now], through its timing curve, in [0, 1] (beyond
   for an overshooting curve) *)
val progress : 'a t -> now:float -> float
val finished : 'a t -> now:float -> bool

(* [value ~lerp a ~now]: where it is; [lerp a b p] the value a fraction
   [p] of the way from [a] to [b] *)
val value : lerp:('a -> 'a -> float -> 'a) -> 'a t -> now:float -> 'a

val lerp_float : float -> float -> float -> float

(* implicit animations, by key *)
type ('k, 'a) keyed

val keyed : ?timing:Timing.t -> duration:float -> lerp:('a -> 'a -> float -> 'a) -> unit -> ('k, 'a) keyed

(* [set k ~now key target]: the target; a new key is there at once, a
   changed target animated from where the key is now *)
val set : ('k, 'a) keyed -> now:float -> 'k -> 'a -> ('k, 'a) keyed

(* [value_of k ~now key]: where it is now, if the key is known *)
val value_of : ('k, 'a) keyed -> now:float -> 'k -> 'a option

(* whether any key is still moving (a program may repaint only then) *)
val moving : ('k, 'a) keyed -> now:float -> bool
