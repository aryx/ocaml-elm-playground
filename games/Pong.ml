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

let initial_model = ()

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view _computer _model = []

(*****************************************************************************)
(* Update *)
(*****************************************************************************)
let update _msg model =
  model

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  game view update initial_model

let main = 
  Playground_platform.run_app app



