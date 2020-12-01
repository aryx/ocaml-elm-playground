type t =
  | Hex of string
  | Rgb of int * int * int

let (color_clamp : int -> int) = fun number ->
  Basics.clamp 0 255 number

let (rgb : int -> int -> int -> t) = fun r g b ->
  Rgb (color_clamp r, color_clamp g, color_clamp b)


let white = Hex "#FFFFFF"
let black = Hex "#000000"

let red   = Hex "#cc0000"
let green = Hex "#73d216"
let blue  = Hex "#3465a4"

let yellow = Hex "#edd400"

let brown = Hex "#c17d11"

(*-------------------------------------------------------------------*)
(* Light colors *)
(*-------------------------------------------------------------------*)

let lightYellow = Hex "#fce94f"
let lightPurple = Hex "#ad7fa8"

(*-------------------------------------------------------------------*)
(* Dark colors *)
(*-------------------------------------------------------------------*)

(*-------------------------------------------------------------------*)
(* Shades of grey *)
(*-------------------------------------------------------------------*)

let gray = Hex "#d3d7cf"

let darkGray = Hex "#babdb6"
