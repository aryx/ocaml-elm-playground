open Playground

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the Snake clone from https://github.com/amarantedaniel/snek
 * using OCaml instead of Elm, and using Playground instead of HTML/SVG.
 *)

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

(* The origin of the grid (0, 0) is at the bottom left of the screen.
 * This is different from the coordinate system of Playground where the
 * origin is at the center of the screen, but it allows to use mod_by
 * to easily move the snake around the corners.
 *)
type position = (int * int)

type grid_size = {
  g_width: int;
  g_height: int;
}
(* less: could be changed *)
let grid_size = { g_width = 20; g_height = 20 }

let cell_size screen = 
  int_of_float screen.width / grid_size.g_width
 
let random_position () =
  (Random.int (grid_size.g_width - 1), Random.int (grid_size.g_height - 1))

type direction = Up | Down | Left | Right

(* using mutable so easier to update subparts of the model *)
type snake = {
    mutable head: position;
    mutable body: position list;
    mutable direction: direction;
}
let initial_snake = {
    head = (3, 0);
    body = [(2, 0); (1, 0); (1, 0)];
    direction = Right;
}


type model = {
    snake: snake;
    mutable food: position;
    mutable game_over: bool;
}
let initial_model = {
    snake = initial_snake;
    food = (grid_size.g_width / 2, grid_size.g_height / 2);
    game_over = false;
}

(*****************************************************************************)
(* View *)
(*****************************************************************************)
let f = float
let i = int_of_float
let smaller size = f size *. 0.90

let movei a b shape = move (f a) (f b) shape

let translate (x,y) screen shape =
  let cell_size = cell_size screen in
  shape 
  |> move screen.left screen.bottom
  |> movei (x * cell_size) (y * cell_size)
  |> movei (cell_size / 2) (cell_size / 2)


let view_background screen = 
  [rectangle (Color.Hex "#8cbf00") screen.width screen.height]

let view_food screen pos = 
  let size = cell_size screen in
  let radius = size / 3 in
  [circle gray (f radius) |> translate pos screen;
   circle black (smaller radius) |> translate pos screen;
  ]

let view_snake_part screen pos =
  let size = cell_size screen in
  [square gray (f size) |> translate pos screen;
   square black (smaller size) |> translate pos screen;
  ]

let view_snake screen snake =
  List.map (view_snake_part screen) (snake.head::snake.body) |> List.flatten

(* TODO: use words red, bold, fontsize 50 *)
let view_game_over _screen =
  [ square blue 40.]

let view computer model = 
  let screen = computer.screen in
  if model.game_over
  then view_game_over screen
  else view_background screen @
       view_snake screen model.snake @
       view_food screen model.food
  

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
  Random.self_init ();
  Playground_platform.run_app app
