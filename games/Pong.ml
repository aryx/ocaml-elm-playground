open Playground
open Basics (* float operators by default *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the Pong clone described at https://elm-lang.org/news/making-pong
 * but using OCaml instead of Elm, and using Playground instead of Collage.
 *
 * See https://en.wikipedia.org/wiki/Pong for more information on Pong.
 *
 * TODO:
 *  - http://mathieu.agopian.info/blog/making-a-pong-game-in-elm.html (2019)
 *  - https://github.com/einars/skapong ocaml one
 *  - https://www.awesomeinc.org/tutorials/unity-pong/ unity! with
 *    physics engine (friction, bounciness)
 *  - https://en.scratch-wiki.info/wiki/Pong
 *  - https://www.101computing.net/pong-tutorial-using-pygame-getting-started/
 *
 * TODO: extensions listed at the end of the making-pong blog post:
 *  - dotted line mid field
 *  - ability to pause the game
 *  - ability to reset the game (menu somewhere? restart button between
 *    the score)
 *  - make ball collisions more complicated:
 *    * when the ball hits the corner of a paddle, it changes direction
 *    * if the ball hits a moving paddle, it adds spin to the ball, making it
 *      rebound in a different direction
 *  - add a second ball to the game
 *  - write a simple AI for a paddle (easy but boring = AI that try to
 *    stay at the same y than the ball; but could design less boring but
 *    more fun AI too).
 *)

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

(* was time in Pong.elm, but playground defines time as Time of posix *)
type delta = float 

type obj = { 
    x: number; 
    y: number; 
    vx: number;
    vy: number;
}

type ball = obj

type player = {
    obj: obj;
    score: int;
}
let player x = 
  { obj = { (* should remain constant *)
            x; 
            y = 0.; 
            (* will remain 0 *)
            vx = 0.; 
            vy = 0. }; 
     score = 0 }

type state = Play | Pause

type game = {
    state: state;
    ball: ball;
    player1: player;
    player2: player;
}

(* coupling: with Playground.initial_computer.screen at 600 x 600 *)
let (game_width, game_height) = (600., 400.)
let (half_width, half_height) = (300., 200.)

(* This means that in 1 second, the object will move 200 pixels.
 * note: was duplicated many times in the blog post, better to factorize.
 *)
let default_velocity = 200.

let default_game = { 
    state = Pause;
    ball = { x = 0.; y = 0.; vx = default_velocity; vy = default_velocity };
    (* player1 is on the left, player2 on the right *)
    player1 = player (-. half_width + 20.);
    player2 = player (   half_width - 20.);
}

(*****************************************************************************)
(* View *)
(*****************************************************************************)
let pong_green = Color.Rgb (60, 100, 60)
let text_green = Color.Rgb (160, 200, 160)

let msg = "SPACE to start, w/s and up/down to move"

let display_obj obj shape = 
  shape |> move (obj.x) (obj.y)

let view _computer (game, _last_tick) =
  [ rectangle pong_green game_width game_height;

    display_obj game.ball (oval white 15. 15.);

    display_obj game.player1.obj (rectangle white 10. 40.);
    display_obj game.player2.obj (rectangle white 10. 40.);

    (let s = Printf.sprintf "%d   %d" game.player1.score game.player2.score in
    words text_green s |> scale 10. |> move 0. (game_height / 2. - 40.));
    
    (let s = if game.state = Play then "" else msg in
    words text_green s |> scale 2. |> move 0. (40. - game_height / 2.));
  ]

(*****************************************************************************)
(* Update *)
(*****************************************************************************)
(* are n and m near each other, specifically are they within c of each other *)
let near n c m = 
  m >= n - c && m <= n + c

(* Is the ball within a paddle?
 * coupling: 8 is (a little more than) half of oval width (15) of the ball 
 * and 20 is half the height of the player paddle (40)
 *)
let (within: ball -> player -> bool) = fun ball player ->
    near player.obj.x 8. ball.x &&
    near player.obj.y 20. ball.y

(* change the direction of a velocity (vx, or vy) based on collisions *)
let (step_v: number -> bool -> bool -> number) = 
 fun v lower_collision upper_collision ->
  match () with
  (* bottom or left collision *)
  | _ when lower_collision -> abs_float v
  (* top or right collision *)
  | _ when upper_collision -> -. (abs_float v)
  | _ -> v

let (step_obj: delta -> obj -> obj) = fun t ({ x; y; vx; vy} as obj) ->
  { obj with x = x + vx * t; y = y + vy * t }

(* move a ball forward, detecting collisions with either paddle *)
let (step_ball: delta -> ball -> player -> player -> ball) =
 fun t ({x = _; y; vx; vy} as ball) player1 player2 ->
  (* put back at the center of the screen when reach player side *)
  if not (near 0. half_width ball.x)
  then { ball with x = 0.; y = 0. }
  else
    step_obj t { ball with
      vx = step_v vx (within ball player1) (within ball player2);
      (* coupling: 7. =~ half size of ball *)
      vy = step_v vy (y < -. half_height + 7.) (y > half_height - 7.);
    }

(* step a player forward, making sure it does not fly off the screen *)    
let (step_player: delta -> number -> int -> player -> player) =
  fun t dir points player ->
    let obj' = 
      (* bugfix: vy here! not vx *)
      step_obj t { player.obj with vy = dir * default_velocity } in
    let y' = Basics.clamp (-. half_height + 22.) (half_height - 22.) obj'.y in
    let score' = player.score +.. points in
    { obj = { obj' with y = y'}; score = score' }

type input = {
    space: bool;

    (* -1, 0, 1 *)
    paddle1: number;
    paddle2: number;

    delta: delta;
}

let input_of_computer computer =
  let kbd = computer.keyboard in
  { space = kbd.kspace;
    (* bugfix: player2 is on the right, so the arrows or for player2 *)
    paddle1 = to_y2 kbd;
    paddle2 = to_y kbd;
    delta = 0.;
  }

let (step_game: input -> game -> game) = fun input game ->
  let { space; paddle1; paddle2; delta} = input in
  let {state; ball; player1; player2} = game in

  (* ball on the right of player2 => score for player 1 *)
  let score1 = if ball.x > half_width then 1 else 0 in
  (* ball on left of player1 => score for player 2 *)
  let score2 = if ball.x < -. half_width then 1 else 0 in

  let state' =
    match () with
    | _ when space -> Play
    | _ when score1 <> score2 -> Pause
    | _ -> state
  in
  let ball' =
    if state = Pause 
    then ball
    else step_ball delta ball player1 player2
  in
  let player1' = step_player delta paddle1 score1 player1 in
  let player2' = step_player delta paddle2 score2 player2 in
  { state = state'; ball = ball'; player1 = player1'; player2 = player2' }

let update computer (game, last_tick) =
  let input = input_of_computer computer in
  let (Time now) = computer.time in
  let delta = now - last_tick in
  let game' = step_game { input with delta } game in
  game', now

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  game view update (default_game, Unix.gettimeofday())

let main = 
  Playground_platform.run_app app
