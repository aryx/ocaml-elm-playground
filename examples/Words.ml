open Playground

let app =
  picture [
    rectangle green 10. 10. |> scale 2. |> move 20. 20. |> rotate 45.;
    rectangle black 2. 2.;
    words green "foobar" |> scale 2. |> rotate 90.;
  ]

let main = Playground_platform.run_app app
