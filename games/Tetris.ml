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
type color = Color.t

(* pad: I introduced this type.
 * The origin (0, 0) in the grid is at the top left corner on the screen.
 * Pos can also have a negative y, meaning it is not yet visible.
 *)
type pos = {x: int; y: int }

(* orig: was polymorphic, but always use with color, so simpler to hardcode *)
type cell = {
  color: color;
  pos: pos;
}

(* an empty cell is represented as a non-existing cell *)
type grid = cell list

let empty_grid = []

(* pad: I introduced this type *)
type piece = grid

let (center_of_mass: piece -> pos) = fun _piece ->
    failwith "TODO"

let (init_position: int -> piece -> pos) = fun wid piece ->
    let {x; _} = center_of_mass piece in
    let y = piece |> List.map (fun cell -> cell.pos.y) |> Common2.maximum in
    { x = wid / 2 - x; y = - y - 1 }




let (from_list: color -> (int * int) list -> grid) = fun color xs ->
    xs |> List.map (fun (x, y) -> { color; pos = {x ; y} })

let (tetriminos: piece list) = [
  (* xxxx 
   *)
  Color.Rgb (60, 199, 214), [(0, 0); (1, 0); (2, 0); (3, 0)];
  (* xx
   * xx
   *)
  Color.Rgb (251, 180, 20), [(0, 0); (1, 0); (0, 1); (1, 1)];
  (* xxx
   *  x
   *)
  Color.Rgb (176, 68, 151), [(1, 0); (0, 1); (1, 1); (2, 1)];
  (* xxx
   * x
   *)
  Color.Rgb (57, 147, 208), [(0, 0); (0, 1); (1, 1); (2, 1)];
  (* xxx
   *   x
   *)
  Color.Rgb (237, 101, 47), [(2, 0); (0, 1); (1, 1); (2, 1)];
  (* xx
   *  xx
   *)
  Color.Rgb (149, 196, 61), [(1, 0); (2, 0); (0, 1); (1, 1)];
  (*  xx
   * xx
   *)
  Color.Rgb (232, 65, 56),  [(0, 0); (1, 0); (1, 1); (2, 1)];
 ] |> List.map (fun (color, xs) -> from_list color xs)

let random_tetrimino () =
  let len = List.length tetriminos in
  let n = Random.int len in
  List.nth tetriminos n



(* todo? need Stopped and Paused? *)
type state = 
  | Stopped
  | Playing
  | Paused

(* todo? animationState? size? *)
type model = {
  width: int;
  height: int;

  score: int;
  lines: int;

  (* current state of the grid *)
  grid: grid;

  (* current piece *)
  active: piece;
  position: (int * float); (* todo: float??*)
  (* next piece *)
  next: piece;

  state: state;
  (* todo: last_tick? *)
}

let spawn_tetrimino model =
  let active = model.next in
  let next = random_tetrimino () in
  let {x; y} = init_position model.width active in
  { model with next; active; position = (x, float y) }

let initial_model = spawn_tetrimino {
    width = 10;
    height = 20;

    score = 0;
    lines = 0;

    grid = empty_grid;

    active = empty_grid;
    position = (0, 0.);
    next = random_tetrimino ();

    state = Stopped;
  }
    

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
  Random.self_init ();
  Playground_platform.run_app app
