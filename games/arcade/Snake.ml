open Playground

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the Snake clone https://github.com/amarantedaniel/snek,
 * but using OCaml instead of Elm, and using Playground instead of HTML/SVG.
 *
 * See https://en.wikipedia.org/wiki/Snake_(video_game_genre) for more info.
 *
 * claude: two sounds (Audio): a crunch when the snake eats,
 * a short burst of noise and a rising blip at once; and a falling
 * tone, the end, when it bites itself.
 *
 * claude: juice (Juice.mli): the food pops in where it
 * appears, growing from nothing and overshooting a little; eaten, it
 * bursts into black crumbs and sparks, the screen shakes a little and
 * the snake's head gulps (squashes and springs back); the snake biting
 * itself shakes the screen hard and flashes it red. The juice is on by
 * default; the flag juice=off gives the dry game (dune exec
 * games/arcade/Snake.exe -- juice=off). The effects watch the game from
 * outside ([juiced]), and draw nothing from Random: the same keys and
 * seed give the same game, dry or juiced. Hitstop (Juice.freeze) is
 * left out on purpose: it would change when the snake moves.
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
    (* claude: the juice: the effects, when the food appeared (it pops
     * in), when the snake last ate (its head gulps) *)
    fx: Juice.t;
    food_shown: time;
    ate: time;
}
let initial_model = {
    snake = initial_snake;
    food = (grid_size.g_width / 2, grid_size.g_height / 2);
    game_over = false;
    last_tick = 0.;
    fx = Juice.none ~seed:1;
    food_shown = Time 0.;
    ate = Time (-10.);
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec list_init = function
  | [] -> raise Not_found
  | [ _x ] -> []
  | x :: y :: xs -> x :: list_init (y :: xs)

(*****************************************************************************)
(* The juice (juice=off: none of it) *)
(*****************************************************************************)

(* claude: everything the juice does is here, and the rules below don't
 * know about it: [update] runs them, then [juiced] looks at what they
 * just did -- the food, the snake's length, the game over, before and
 * after -- and turns it into effects; the view calls [pop] and [gulp]
 * where it draws the food and the head, and [Juice.view] around the
 * picture. *)

(* the center of a cell, in Playground's coordinates (as the view's
 * translate puts it) *)
let cell_center (screen : screen) ((x, y) : position) : number * number =
  let size = cell_size screen in
  (screen.left +. float ((x * size) + (size / 2)), screen.bottom +. float ((y * size) + (size / 2)))

(* [before] is a copy of the model taken before the rules ran, but the
 * snake is shared, mutated in place: its length is taken apart *)
let juiced (screen : screen) (before : model) (length : int) (model : model) : model =
  let now = Juice.now model.fx in
  (* eaten: the snake grew; the food bursts where it was (under the new
   * head), the screen shakes a little, the head gulps, and the new food
   * pops in *)
  let model =
    if List.length model.snake.body > length then
      let at = cell_center screen model.snake.head in
      let fx = model.fx |> Juice.shake 0.3 |> Juice.burst ~at (Juice.debris black) |> Juice.burst ~at Juice.sparks in
      { model with fx; ate = now; food_shown = now }
    else model
  in
  (* bitten: the end *)
  if model.game_over && not before.game_over then { model with fx = model.fx |> Juice.shake 0.8 |> Juice.flash red 20 }
  else model

(* the food's size: popping in from nothing when it appears *)
let pop (model : model) : number = Juice.tween Juice.out_back 0. 1. 0.3 model.food_shown model.fx

(* the head, squashed the moment it ate and springing back (a cell
 * centered on (0, 0), squashed about its middle) *)
let gulp (model : model) (shape : shape) : shape = Juice.stretch (Juice.squash 0.4 0.3 model.ate model.fx) shape

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

let view_food model screen pos =
  let size = cell_size screen in
  let radius = size / 3 in
  (* claude: the juice: popping in *)
  [circle gray (f radius) |> scale (pop model) |> translate pos screen;
   circle black (smaller radius) |> scale (pop model) |> translate pos screen;
  ]

let view_snake_part ?(juice = fun shape -> shape) screen pos =
  let size = cell_size screen in
  [square gray (f size) |> juice |> translate pos screen;
   square black (smaller size) |> juice |> translate pos screen;
  ]

let view_snake model screen snake =
  (* claude: the juice: the head gulps *)
  view_snake_part ~juice:(gulp model) screen snake.head @
  (List.map (view_snake_part screen) snake.body |> List.flatten)

let view_game_over _screen =
  [ words red "GAME OVER" |> scale 10. ]

let view computer model = 
  let screen = computer.screen in

  view_background screen @
  (* claude: the background still, everything else shaken (the juice) *)
  Juice.view model.fx (
    view_snake model screen model.snake @
    view_food model screen model.food @
    (if model.game_over then view_game_over screen else []))
  

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

(* claude: the sounds *)
let crunch =
  Audio.together
    [ Audio.sfx { Sfx.hit with decay = 0.05; volume = 0.35 }; Audio.sfx { Sfx.blip with frequency = 660.; slide = 990.; volume = 0.35 } ]

let game_over_sound = Audio.sfx { Sfx.default with wave = Triangle; frequency = 440.; slide = 110.; sustain = 0.2; decay = 0.4 }

let update_rules computer model =
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
      then begin
        model.food <- random_position ();
        Audio.play crunch
      end;
      let bitten = List.mem new_head new_body in
      if bitten && not model.game_over then Audio.play game_over_sound;
      model.game_over <- bitten;
  end;
  update_direction computer.keyboard model.snake;

  model

(* claude: the rules, then the juice watching them *)
let update computer model =
  let model = { model with fx = Juice.step computer model.fx } in
  (* before the rules run (OCaml evaluates arguments right to left) *)
  let before = { model with food = model.food } in
  let length = List.length model.snake.body in
  juiced computer.screen before length (update_rules computer model)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  game view update initial_model

let main =
  (* claude: seed=n (see Playground.flags), e.g. for the golden frame
   * tests: the same food positions every run *)
  (match List.assoc_opt "seed" (Playground_platform.flags ()) with
  | Some n -> Random.init (int_of_string n)
  | None -> Random.self_init ());
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
