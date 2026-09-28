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


(* claude: two physics engines for the same Mario, chosen with the flag
 * physics=engine (?physics=engine in a browser; see Playground.flags),
 * the dumb one by default:
 *
 *  - the dumb engine, Evan's original code below: velocities in pixels
 *    per 1/100 of a second (x moves dt * vx per frame, dt = 1.666, so
 *    vx = 1 is 100 pixels per second), gravity subtracting dt / 8 from
 *    vy at every frame;
 *  - the physics engine (Physics.mli): the same Mario as a
 *    body, in pixels and seconds -- walking at 100 px/s, jumping at
 *    500 px/s, falling at 1250 px/s^2 (the same numbers, converted).
 *
 * They move the same, because the dumb engine *is* a physics engine: it
 * changes vy first, then moves y with the new vy -- semi-implicit Euler,
 * exactly what Physics.step does (docs/claude_notes/notes_2d_physics.md
 * section 4). What the physics engine adds is names (fall, step), units
 * a game can reason about, and the rest of the engine when a game needs
 * it (collisions, bouncing, gravitation). *)
let update_dumb computer mario =
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

let update_physics computer mario =
  let vx = to_x computer.keyboard in
  let on_ground = mario.y = 0. in
  let body =
    Physics.body (square white 1.)
    |> Physics.at mario.x mario.y
    |> Physics.moving (100. * vx) (if on_ground then (if computer.keyboard.kup then 500. else 0.) else 100. * mario.vy)
  in
  (* gravity only in the air: on the ground, it's the ground pushing back *)
  let body = (if on_ground then body else Physics.fall 1250. body) |> Physics.step in
  { x = body.x;
    y = max 0. body.y;
    vx;
    vy = body.vy / 100.; (* back in the model's units, per 1/100 s *)
    dir = if vx = 0. then mario.dir else if vx < 0. then "left" else "right"
  }

let update computer mario =
  match List.assoc_opt "physics" computer.flags with
  | Some "engine" -> update_physics computer mario
  | _ -> update_dumb computer mario

let app = game view update initial_model

(* claude: pixel art: keep the sprites' pixels sharp when enlarged, on
 * every backend (see Playground.rendering); and the network granted,
 * its sprites coming from elm-lang.org (plan_caps.md) *)
let main =
  Cap.main (fun caps ->
      Playground_platform.run_app ~rendering:{ default_rendering with smooth_images = false }
        ~flags:(Playground_platform.flags ()) ~network:caps app)

