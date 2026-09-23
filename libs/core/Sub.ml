type 'msg onesub =  
  | SubTick of (Time.posix -> 'msg)
  | SubMouseMove of (float * float -> 'msg)
  (* pad: not in Elm: relative motion, dx and dy (y up) *)
  | SubMouseMoveBy of (float * float -> 'msg)
  | SubMouseDown of (unit -> 'msg)
  | SubMouseUp of (unit -> 'msg)
  (* pad: not in Elm (its onMouseDown gives the event, with its button) *)
  | SubRightMouseDown of (unit -> 'msg)
  | SubRightMouseUp of (unit -> 'msg)
  | SubKeyDown of (Keyboard.key -> 'msg)
  | SubKeyUp of (Keyboard.key -> 'msg)
  (* claude: the three an application needs and a game never did (see
   * docs/claude_notes/plans/plan_gui_teaching.md, phase 0): the
   * characters a key press produces (a key name is not a character:
   * shift, dead keys and layouts are the platform's business), the
   * wheel, and the double click *)
  | SubTyped of (string -> 'msg)
  | SubMouseWheel of (float -> 'msg)
  | SubMouseDouble of (unit -> 'msg)


type 'msg t = 'msg onesub list
let none = []
let batch xs = (List.flatten xs)

(* was in Event_.ml before *)
let (on_animation_frame: (Time.posix -> 'msg) -> 'msg t) = fun f ->
  [SubTick f]

let (on_mouse_move: (float * float -> 'msg) -> 'msg t) = fun f ->
  [SubMouseMove f]

let (on_mouse_move_by: (float * float -> 'msg) -> 'msg t) = fun f ->
  [SubMouseMoveBy f]

let (on_mouse_down: (unit -> 'msg) -> 'msg t) = fun f ->
  [SubMouseDown f]

let (on_mouse_up: (unit -> 'msg) -> 'msg t) = fun f ->
  [SubMouseUp f]

(* claude: on_mouse_down/on_mouse_up are the left (main) button *)
let (on_right_mouse_down: (unit -> 'msg) -> 'msg t) = fun f ->
  [SubRightMouseDown f]

let (on_right_mouse_up: (unit -> 'msg) -> 'msg t) = fun f ->
  [SubRightMouseUp f]

let (on_key_down: (Keyboard.key -> 'msg) -> 'msg t) = fun f ->
  [SubKeyDown f]

let (on_key_up: (Keyboard.key -> 'msg) -> 'msg t) = fun f ->
  [SubKeyUp f]

let (on_typed: (string -> 'msg) -> 'msg t) = fun f ->
  [SubTyped f]

let (on_mouse_wheel: (float -> 'msg) -> 'msg t) = fun f ->
  [SubMouseWheel f]

let (on_mouse_double: (unit -> 'msg) -> 'msg t) = fun f ->
  [SubMouseDouble f]




type event = 
  | ETick of float
  | EMouseMove of (int * int)
  | EMouseMoveBy of (float * float) (* dx, dy, y up *)
  | EMouseButton of bool (* is_down = true *)
  | ERightMouseButton of bool (* is_down = true *)
  | EKeyChanged of (bool (* down = true *) * Keyboard.key)
  (* claude: the characters typed, not the keys pressed *)
  | ETyped of string
  (* claude: notches up (positive) or down since the last frame *)
  | EMouseWheel of float
  | EMouseDouble

let rec find_map_opt f = function
  | [] -> None
  | x::xs ->
      (match f x with
      | None -> find_map_opt f xs
      | Some x -> Some x
      )

let event_to_msgopt event subs =
  match event with
  | ETick time ->
      subs |> find_map_opt (function 
       | SubTick f -> Some (f time) 
       | _ -> None
      )
  | EMouseMove (x, y) ->
      subs |> find_map_opt (function 
        | SubMouseMove f ->
          Some (f (float_of_int x, float_of_int y))
         | _ -> None
      )
  | EMouseMoveBy d ->
      subs |> find_map_opt (function
        | SubMouseMoveBy f -> Some (f d)
        | _ -> None
      )
  | EMouseButton (true) ->
      subs |> find_map_opt (function 
        | SubMouseDown f ->
           Some (f ())
       | _ -> None
      )
  | EMouseButton (false) ->
      subs |> find_map_opt (function
        | SubMouseUp f ->
           Some (f ())
       | _ -> None
      )
  | ERightMouseButton (true) ->
      subs |> find_map_opt (function
        | SubRightMouseDown f ->
           Some (f ())
       | _ -> None
      )
  | ERightMouseButton (false) ->
      subs |> find_map_opt (function
        | SubRightMouseUp f ->
           Some (f ())
       | _ -> None
      )
  | EKeyChanged (true, key) ->
      subs |> find_map_opt (function 
        | SubKeyDown f ->
           Some (f key)
       | _ -> None
      )
  | EKeyChanged (false, key) ->
      subs |> find_map_opt (function 
        | SubKeyUp f ->
           Some (f key)
       | _ -> None
      )
  | ETyped str ->
      subs |> find_map_opt (function
        | SubTyped f -> Some (f str)
        | _ -> None
      )
  | EMouseWheel notches ->
      subs |> find_map_opt (function
        | SubMouseWheel f -> Some (f notches)
        | _ -> None
      )
  | EMouseDouble ->
      subs |> find_map_opt (function
        | SubMouseDouble f -> Some (f ())
        | _ -> None
      )
