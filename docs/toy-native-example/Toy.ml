open Playground

(* the (x, y) position of the blue square  *)
type model = (float * float)

let initial_state : model = (0., 0.)

let view _computer (x, y) = [ 
  square blue 40.
   |> move x y
 ]

let update computer (x, y) =
  (x +. to_x computer.keyboard, y +. to_y computer.keyboard)

let app = 
  game view update initial_state

let main = Playground_platform.run_app app
