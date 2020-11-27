open Playground

let star =
      group
        [ triangle yellow 20.;
          triangle yellow 20.
            |> rotate 180.;
        ]

let app =
  picture [
    polygon black [ (-10., -20.); (0., 100.); (10., -20.)];
    rectangle green 10. 10. |> scale 2. |> move 20. 20. |> rotate 45.;

    star |> move 100. 100. |> rotate 5.;
    star |> move (-120.) 40. |> rotate 20.;
    star |> move 80. (-150.) |> rotate 32.;
    star |> move (-90.) (-30.) |> rotate (-16.);
  ]

let main = Playground_platform.run_app app
