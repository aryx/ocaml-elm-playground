(* move in Sub? coupling with Sub.t type anyway *)
type t = 
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
