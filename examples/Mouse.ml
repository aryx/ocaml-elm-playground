(* from https://elm-lang.org/examples/mouse *)
open Playground

let view computer _memory = [ 
  rectangle yellow computer.screen.width computer.screen.height;
  circle lightPurple 30.
   |> move_x computer.mouse.mx
   |> move_y computer.mouse.my
   |> fade (if computer.mouse.mdown then 0.2 else 1.)
 ]

let update _computer () =
  ()

let app = 
  game view update ()

let main = Playground_platform.run_app app
