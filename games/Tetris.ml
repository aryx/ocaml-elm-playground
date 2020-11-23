open Playground

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the Tetris clone https://github.com/w0rm/elm-flatris 
 * (itself a clone of https://github.com/skidding/flatris, 
 *  itself a clone of the venerable Tetris), 
 * but using OCaml instead of Elm, and using Playground instead of HTML/SVG.
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
 * The origin (0, 0) in the grid is at the top left corner of the tetris "well".
 * Pos can also have a negative y, meaning it is not yet visible.
 *)
type pos = {x: int; y: int }


(* orig: was polymorphic, but always use with color, so simpler to hardcode *)
type cell = {
  color: color;
  pos: pos;
}

(* an empty cell is represented as a non-existing cell.
 * alt: cell option array.
*)
type grid = cell list

let empty_grid = []


(* pad: I introduced this type *)
type piece = grid

let (center_of_mass: piece -> pos) = fun piece ->
    let len = float (List.length piece) in
    let xs = piece |> List.map (fun cell -> cell.pos.x) in
    let ys = piece |> List.map (fun cell -> cell.pos.y) in
    { x = Basics.round (float (Common2.sum xs) /. len);
      y = Basics.round (float (Common2.sum ys) /. len);
    }

let (init_position: int -> piece -> pos) = fun wid piece ->
    let {x; _} = center_of_mass piece in
    let y = piece |> List.map (fun cell -> cell.pos.y) |> Common2.maximum in
    (* -y -1 to set as non visible the bottom of the piece *)
    { x = wid / 2 - x; 
      y = - y - 1 
    }

let (size: piece -> (int * int)) = fun piece ->
    let xs = piece |> List.map (fun cell -> cell.pos.x) in
    let ys = piece |> List.map (fun cell -> cell.pos.y) in
    let dim zs = 1 + Common2.maximum zs in
    dim xs, dim ys

let rec (stamp: pos -> piece -> grid -> grid) = fun { x; y} piece grid ->
  match piece with
  | [] -> grid
  | cell::rest ->
    let newpos = { x = cell.pos.x + x; y = cell.pos.y + y } in
    (* todo: need exclude from grid cells with pos = newpos? *)
    stamp { x; y } rest ({ cell with pos = newpos }::grid)


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

(* todo? animationState? *)
type model = {
  (* screen size (= Playground.initial_computer.screen) *)
  (* size: number * number; *)

  (* grid dimension *)
  width: int;
  height: int;

  (* current state of the grid *)
  grid: grid;

  (* current piece *)
  active: piece;
  position: (int * float); (* todo: float??*)
  (* next piece *)
  next: piece;

  score: int;
  lines: int;

  state: state;
  
  last_tick: float;
}

let spawn_tetrimino model =
  let active = model.next in
  let next = random_tetrimino () in
  let {x; y} = init_position model.width active in
  { model with next; active; position = (x, float y) }

let _init = Random.self_init ()

let initial_model = spawn_tetrimino {

    (* coupling: Playground.initial_computer.screen *)
    (* size = (600., 600.); *)

    width = 10;
    height = 20;

    score = 0;
    lines = 0;

    grid = empty_grid;

    active = empty_grid;
    position = (0, 0.);
    next = random_tetrimino ();

    state = Stopped;

    last_tick = Unix.gettimeofday();
  }

(*****************************************************************************)
(* View *)
(*****************************************************************************)
let f = float

(* in pixels, height = 20 * 30 = 600 < 768 height in initial_computer.screen *)
let cell_size = 30

let move_box {width; height; _ } { x; y } shape =
  shape 
  (* go to (0, 0), top left corner of the well *)
  |> move_up (float (height * cell_size / 2)) 
  |> move_left (float (width * cell_size / 2))
  (* now move the piece to (x, y) *)
  |> move_right (float (x * cell_size + cell_size / 2))
  |> move_down  (float (y * cell_size + cell_size / 2))

let render_box model { color; pos } =
  square color (f cell_size) |> move_box model pos

let render_well 
 ({ width; height; position = (posx, posy); active; grid; _ } as model) =
  let final_grid = 
    stamp { x = posx; y = int_of_float (floor posy) } active grid
  in
  [rectangle (Color.Rgb (236, 240, 241)) 
      (f (width * cell_size))
      (f (height * cell_size))
  ] @
  (final_grid |> List.map (render_box model)) @
  []

let view model =
  render_well model @
  []

(*****************************************************************************)
(* Update *)
(*****************************************************************************)
type msg = 
  | Tick of float
  | KeyDown of Keyboard.key
  | KeyUp of Keyboard.key

(* todo: Resize/GetViewPort *)
let update _msg model =
  model,
  Cmd.none

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  { Playground.
    view;
    update;
    init = (fun () -> initial_model, Cmd.none);
    subscriptions  = (fun _ -> Sub.batch [
      Sub.on_animation_frame (fun x -> Tick x);
      Sub.on_key_down (fun key -> KeyDown (key));
      Sub.on_key_up   (fun key -> KeyUp (key));
    ]);
  }

let main = 
  Playground_platform.run_app app
