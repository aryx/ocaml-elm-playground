open Playground

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the Pong clone https://elm-lang.org/news/making-pong
 * using OCaml instead of Elm, and using Playground instead of HTML/SVG??
 *
 * inspiration:
 *  - https://elm-lang.org/news/making-pong (2012)
 *  - http://mathieu.agopian.info/blog/making-a-pong-game-in-elm.html (2019)
 *)

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

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
  { obj = { x; y = 0.; vx = 0.; vy = 0. }; score = 0 }

type state = Play | Pause

type game = {
    state: state;
    ball: ball;
    player1: player;
    player2: player;
}

let game_height = 400.
let game_width = 600.

let default_game screen = { 
    state = Pause;
    ball = { x = 0.; y = 0.; vx = 200.; vy = 200. };
    player1 = player (screen.left +. 20.);
    player2 = player (screen.right -. 20.);
}


(*****************************************************************************)
(* View *)
(*****************************************************************************)
let pong_green = Color.Rgb (60, 100, 60)
let text_green = Color.Rgb (160, 200, 160)

let display_obj obj shape = 
  shape |> move (obj.x) (obj.y)

let view _computer game =
  (* TODO: score *)
  [ rectangle pong_green game_width game_height;
    display_obj game.ball (oval white 15. 15.);
    display_obj game.player1.obj (rectangle white 10. 40.);
    display_obj game.player2.obj (rectangle white 10. 40.);
  ]

(*****************************************************************************)
(* Update *)
(*****************************************************************************)
type input = {
    space: bool;

    (* -1, 0, 1 *)
    paddle1: number;
    paddle2: number;

    delta: time;
}

let input_of_computer computer =
  let kbd = computer.keyboard in
  { space = kbd.kspace;
    paddle1 = to_y kbd;
    paddle2 = to_y2 kbd;
    delta = computer.time;
  }

let update _computer model =
  model

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  game view update (default_game (Playground.initial_computer.screen))

let main = 
  Playground_platform.run_app app



