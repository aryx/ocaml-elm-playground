(* from https://elm-lang.org/examples/picture *)
open Playground

let app =
  picture [
(*
    rectangle brown 40. 200.
      |> move_down 80.;
*)
    circle green 100.
      |> move_up 100.;
  ]

let main = Playground.run_app app
