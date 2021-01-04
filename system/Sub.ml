type 'msg onesub =  
  | SubTick of (Time.posix -> 'msg)
  | SubMouseMove of (float * float -> 'msg)
  | SubMouseDown of (unit -> 'msg)
  | SubMouseUp of (unit -> 'msg)
  | SubKeyDown of (Keyboard.key -> 'msg)
  | SubKeyUp of (Keyboard.key -> 'msg)


type 'msg t = 'msg onesub list
let none = []
let batch xs = (List.flatten xs)

(* was in Event_.ml before *)
let (on_animation_frame: (Time.posix -> 'msg) -> 'msg t) = fun f ->
  [SubTick f]

let (on_mouse_move: (float * float -> 'msg) -> 'msg t) = fun f ->
  [SubMouseMove f]

let (on_mouse_down: (unit -> 'msg) -> 'msg t) = fun f ->
  [SubMouseDown f]

let (on_mouse_up: (unit -> 'msg) -> 'msg t) = fun f ->
  [SubMouseUp f]

let (on_key_down: (Keyboard.key -> 'msg) -> 'msg t) = fun f ->
  [SubKeyDown f]

let (on_key_up: (Keyboard.key -> 'msg) -> 'msg t) = fun f ->
  [SubKeyUp f]




type event = 
  | ETick of float
  | EMouseMove of (int * int)
  | EMouseButton of bool (* is_down = true *)
  | EKeyChanged of (bool (* down = true *) * Keyboard.key)

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
