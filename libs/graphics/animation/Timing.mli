(* Timing: how an animation's time becomes its progress, as Core
   Animation (CAMediaTimingFunction) and CSS (cubic-bezier) say it: a
   cubic Bezier curve from (0, 0) to (1, 1), its two middle control
   points given, x the time's fraction, y the way's.

      way
     1 |            ____.(1, 1)          ease_in_out: (0.42, 0) and
       |        _.-'   P2                (0.58, 1), slow at both ends
       |      .'                         (Core Animation's, CSS's)
       |    .'
       |  .'  P1
     0 +-'-----------------> time
      (0, 0)                1

   A curve with its x and its y both functions of a parameter s:

     B(s) = 3 (1-s)^2 s P1 + 3 (1-s) s^2 P2 + s^3 (1, 1)

   so the way at a time t is y(s) for the s where x(s) = t: x is
   increasing (the control points' x in [0, 1]), and s is found by
   Newton's method from s = t, then by bisection if Newton strays --
   WebKit's UnitBezier does the same.

   Why Beziers rather than Penner's formulas (libs/juice/Ease.mli)? One
   form for every curve, four numbers a designer can drag (Apple's and
   Chrome's curve editors), and y may leave [0, 1] (an overshoot:
   (0.34, 1.56, 0.64, 1)).

   Worked examples (the tests'): linear is the identity; ease_in_out is
   symmetric, 0.5 at 0.5; ease_in at 0.5 is about 0.3153; ease_out at
   0.5 about 0.6847. *)

type t

(* [bezier x1 y1 x2 y2]: the curve through (0,0), (x1,y1), (x2,y2),
   (1,1); x1 and x2 clamped to [0, 1] *)
val bezier : float -> float -> float -> float -> t

(* Core Animation's named curves *)
val linear : t
val ease_in : t (* 0.42, 0, 1, 1 *)
val ease_out : t (* 0, 0, 0.58, 1 *)
val ease_in_out : t (* 0.42, 0, 0.58, 1 *)
val default : t (* 0.25, 0.1, 0.25, 1: CSS's "ease", Core Animation's default *)

(* [at c t]: the way gone at the time's fraction [t], clamped to [0, 1] *)
val at : t -> float -> float
