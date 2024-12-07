open Playground

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the Snake clone https://github.com/amarantedaniel/snek,
 * but using OCaml instead of Elm, and using Playground instead of HTML/SVG.
 *
 * See https://en.wikipedia.org/wiki/Snake_(video_game_genre) for more info.
 *
 * TODO:
 *  - two players (like in original Snake game called Blockade)
 *  - display score
 *  - accelerate games as times goes
 *  - high score table
 *)

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

(* The origin of the grid (0, 0) is at the bottom left of the screen.
 * This is different from the coordinate system of Playground where the
 * origin is at the center of the screen, but it allows to use 'mod'
 * to easily move the snake around the edges.
 *)
type position = (int * int) (* x, y *)

type grid_size = {
  g_width: int;
  g_height: int;
}
(* less: could be changed *)
let grid_size = { g_width = 20; g_height = 20 }

let cell_size screen = 
  int_of_float screen.width / grid_size.g_width

(* TODO: do not return a position already used by the snake *) 
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
    mutable last_tick: Time.posix;
}
let initial_model = {
    snake = initial_snake;
    food = (grid_size.g_width / 2, grid_size.g_height / 2);
    game_over = false;
    last_tick = 0.;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec list_init = function
  | [] -> raise Not_found
  | [ _x ] -> []
  | x :: y :: xs -> x :: list_init (y :: xs)

(*****************************************************************************)
(* View *)
(*****************************************************************************)
let f = float
let i = int_of_float
let smaller size = f size *. 0.90

let movei a b shape = move (f a) (f b) shape

(* TODO: this currently assumes a square screen *)
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

let view_game_over _screen =
  [ words red "GAME OVER" |> scale 10. ]

let view computer model = 
  let screen = computer.screen in

  view_background screen @
  view_snake screen model.snake @
  view_food screen model.food @
  (if model.game_over then view_game_over screen else [])
  

(*****************************************************************************)
(* Update *)
(*****************************************************************************)
let compute_new_head snake =
  let (x, y) = snake.head in
  let h = grid_size.g_height in
  let w = grid_size.g_width in
  match snake.direction with
  | Up -> (x, (y + 1 ) mod h)
  | Down -> (x, (y - 1 + h) mod h)
  | Right -> ((x + 1) mod w, y)
  | Left -> ((x - 1 + w) mod w, y)

let update_direction kbd snake =
  let new_dir =
    match () with
    | _ when kbd.kup -> Up
    | _ when kbd.kdown -> Down
    | _ when kbd.kleft -> Left
    | _ when kbd.kright -> Right
    | _ -> snake.direction
  in
  let new_dir = 
    match new_dir, snake.direction with
    (* invalid transitions *)
    | Left, Right | Right, Left 
    | Up, Down | Down, Up
      -> snake.direction
    | x, _ -> x
  in
  snake.direction <- new_dir

let update computer model =
  let (Time now) = computer.time in
  (* operate by side effect on the model; simpler *)
  if now -. model.last_tick > 0.5
  then begin
      model.last_tick <- now;
      let snake = model.snake in
      let new_head = compute_new_head snake in
      let ate_food = new_head = model.food in
      let new_body = 
        if ate_food
        then snake.body
        else list_init snake.body
      in
      snake.body <- snake.head::new_body;
      snake.head <- new_head;
      if ate_food
      then model.food <- random_position ();
      model.game_over <- List.mem new_head new_body;
  end;
  update_direction computer.keyboard model.snake;

  model

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  game view update initial_model

let main = 
  Random.self_init ();
  Playground_platform.run_app app
