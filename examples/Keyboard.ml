(* from https://elm-lang.org/examples/mouse *)
open Playground

let view _computer (x, y) = [ 
  square blue 40.
   |> move x y
 ]

let update computer (x, y) =
  (x +. to_x computer.keyboard, y +. to_y computer.keyboard)

let app = 
  game view update (0., 0.)

let main = Playground.run_app app
