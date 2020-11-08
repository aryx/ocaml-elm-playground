(* from elm-playground/examples.smiley.elm *)
open Playground

let app =
  picture [
    circle lightYellow 200.;

    (* left eye *)
    circle white 50.
     |> move_left 70.
     |> move_up 50.;
    circle black 10.
     |> move_left 75.
     |> move_up 40.;

    (* right eye *)
    circle white 50.
     |> move_right 70.
     |> move_up 50.;
    circle black 10.
     |> move_right 65.
     |> move_up 40.;

    (* mouth *)
    oval black 180. 40.
     |> move_down 100.;
    oval lightYellow 180. 40.
    |> move_down 90.;

  ]

let main = Playground.run_app app
