open Playground

let app =
  picture [
    words green "foobar" |> scale 2. |> rotate 90.;
  ]

let main = Playground_platform.run_app app
