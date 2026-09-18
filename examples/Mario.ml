(* from https://elm-lang.org/examples/mario *)
open Playground
open Basics (* float arithmetics *)

type model = {
  x: number;
  y: number;
  vx: number;
  vy: number;
  dir: string
}

let initial_model =
    { x = 0.;
      y = 0.;
      vx = 0.;
      vy = 0.;
      dir = "right"
    }

let to_gif mario =
  if mario.y > 0. then
    "https://elm-lang.org/images/mario/jump/" ^ mario.dir ^ ".gif"
  else if mario.vx <> 0. then
    "https://elm-lang.org/images/mario/walk/" ^ mario.dir ^ ".gif"
  else
    "https://elm-lang.org/images/mario/stand/" ^ mario.dir ^ ".gif"

(* claude: like a real game's loading screen, load every sprite variant
 * up front instead of lazily on first use -- on native, loading a
 * not-yet-cached sprite mid-game (e.g. the first jump) blocks the render
 * loop for a while (network fetch + decode), which is fine to pay once
 * here but not acceptable frame after frame during gameplay. On the
 * web backend it starts the downloads early and keeps the images in
 * memory, so switching sprites (e.g. walk -> jump) doesn't flicker
 * (see Playground_platform.preload_image in each backend). *)
let () =
  ["stand"; "walk"; "jump"] |> List.iter (fun state ->
    ["left"; "right"] |> List.iter (fun dir ->
      Playground_platform.preload_image
        ("https://elm-lang.org/images/mario/" ^ state ^ "/" ^ dir ^ ".gif")))

let view computer mario =
  let w = computer.screen.width in
  let h = computer.screen.height in
  let b = computer.screen.bottom in

  [ rectangle (rgb 174 238 238) w h;
    rectangle (rgb 74 163 41) w 100.
      |> move_y b;
    image 70. 70. (to_gif mario)

      |> move mario.x (b + 76. + mario.y)
  ]


let update computer mario =
  let dt = 1.666 in
  let vx = to_x computer.keyboard in
  let vy =
      if mario.y = 0. then
        if computer.keyboard.kup then 5. else 0.
      else
        mario.vy - dt / 8.
  in
  let x = mario.x + dt * vx in
  let y = mario.y + dt * vy in
  { x;
    y = max 0. y;
    vx;
    vy;
    dir = if vx = 0. then mario.dir else if vx < 0. then "left" else "right"
  }

let app = game view update initial_model

(* claude: pixel art: keep the sprites' pixels sharp when enlarged, on
 * every backend (see Playground.rendering) *)
let main =
  Playground_platform.run_app ~rendering:{ default_rendering with smooth_images = false } app

