open Playground

let app =
  picture [
    rectangle green 10. 10. |> scale 2. |> move 20. 20. |> rotate 45.;
  ]

let main = Playground_platform.run_app app
