let log s =
  Printf.printf "%s" s

let (/..) = (/)
let (+..) = (+)
let (-..) = (-)
let ( *.. ) = ( * )

(* prefer float operators as default *)
let (/) = (/.)
let (+) = (+.)
let (-) = (-.)
let ( * ) = ( *. )

let (round: float -> int) = fun f ->
    int_of_float (floor (f +. 0.5))

let mod_by a b =
  b mod a

let pi = Float.pi
let pi2 = 8. *. atan 1.

(* was called just degrees in Basics.elm *)
let degrees_to_radians deg =
  (deg * pi) / 180.

let (turns: float -> float) = fun angle_in_turns ->
    2. * pi * angle_in_turns

let (clamp: 'number -> 'number -> 'number -> 'number) = fun low high number ->
  if number < low then
    low
  else if number > high then
    high
  else
    number
