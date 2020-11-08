(* from https://elm-lang.org/examples/mouse *)
open Playground_platformx

let view computer _memory = [ 
  circle lightPurple 30.
   |> move_x computer.mouse.mx
   |> move_y computer.mouse.my
   |> fade (if computer.mouse.mdown then 0.2 else 1.)
 ]

let update _computer () =
  ()

let app = 
  game view update ()

let main = run_app app
