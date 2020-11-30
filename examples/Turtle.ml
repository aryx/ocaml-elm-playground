open Playground
open Basics

type turtle = {
  x: number;
  y: number;
  angle: number;
}

let initial_turtle = { x = 0.; y = 0.; angle = 0. }

let view computer turtle =
  [ rectangle blue computer.screen.width computer.screen.height;
    rectangle yellow 100. 100.;
    image 96. 96. "/tmp/turtle-0.png"
    |> move turtle.x turtle.y
    |> rotate turtle.angle
  ]

let update computer turtle =
  { x = turtle.x + 
      to_y computer.keyboard * cos (degrees_to_radians turtle.angle);
    y = turtle.y +
      to_y computer.keyboard * sin (degrees_to_radians turtle.angle);
    angle = turtle.angle - to_x computer.keyboard
  }

let app = game view update initial_turtle

let main = Playground_platform.run_app app
