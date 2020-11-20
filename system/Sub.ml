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
