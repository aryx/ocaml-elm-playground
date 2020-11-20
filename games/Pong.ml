open Playground

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of a Pong clone 
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

let default_game screen = { 
    state = Pause;
    ball = { x = 0.; y = 0.; vx = 200.; vy = 200. };
    player1 = player (screen.left +. 20.);
    player2 = player (screen.right -. 20.);
}

let initial_model = ()

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view _computer _model = []

(*****************************************************************************)
(* Update *)
(*****************************************************************************)


let update _computer model =
  model

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  game view update initial_model

let main = 
  Playground_platform.run_app app



