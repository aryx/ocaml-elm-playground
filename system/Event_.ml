(* OCaml already defines a stdlib event.ml module for threads, hence Event_ *)

type visibility = 
  | Visible

let (on_animation_frame: (Time.posix -> 'msg) -> 'msg Sub.t) = fun f ->
  [Sub.SubTick f]

let (on_mouse_move: (float * float -> 'msg) -> 'msg Sub.t) = fun f ->
  [Sub.SubMouseMove f]

let (on_mouse_down: (unit -> 'msg) -> 'msg Sub.t) = fun f ->
  [Sub.SubMouseDown f]

let (on_mouse_up: (unit -> 'msg) -> 'msg Sub.t) = fun f ->
  [Sub.SubMouseUp f]

let (on_key_down: (Keyboard.key -> 'msg) -> 'msg Sub.t) = fun f ->
  [Sub.SubKeyDown f]

let (on_key_up: (Keyboard.key -> 'msg) -> 'msg Sub.t) = fun f ->
  [Sub.SubKeyUp f]
