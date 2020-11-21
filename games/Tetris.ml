open Playground

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the Tetris clone https://github.com/w0rm/elm-flatris (itself
 * a clone of https://github.com/skidding/flatris, itself a clone of the
 * venerable Tetris), 
 * but using OCaml instead of Elm, and using Playground instead of SVG.
 *
 * See https://en.wikipedia.org/wiki/Tetris for more information on Tetris.
 *
 * TODO:
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
