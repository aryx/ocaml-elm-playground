(* from https://elm-lang.org/examples/animation *)
open Playground

let view time = 
  Common.pr2 (Common.spf "view: time %s" (Common.dump  time));
  [
    octagon darkGray 36.
      |> move_left 100.
      |> rotate (spin 3. time);
    octagon darkGray 36.
      |> move_right 100.
      |> rotate (spin 3. time);
    rectangle red 300. 80.
      |> move_up (wave 50. 54. 2. time)
      |> rotate (zigzag (-. 2.) 2. 8. time);
  ]

let app =
  animation view

let main = Playground_platform.run_app app
