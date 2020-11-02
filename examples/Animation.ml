(* from https://elm-lang.org/examples/animation *)
open Playground

let view _time = 
  [rectangle red 300. 80.]

let app =
  animation view

let main = Playground.run_app app
