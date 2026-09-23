type t =
  | Hex of string
  | Rgb of int * int * int

let (color_clamp : int -> int) = fun number ->
  Basics.clamp 0 255 number

let (rgb : int -> int -> int -> t) = fun r g b ->
  Rgb (color_clamp r, color_clamp g, color_clamp b)


let white = Hex "#FFFFFF"
let black = Hex "#000000"

let red    = Hex "#cc0000"
let orange = Hex "#f57900"
let yellow = Hex "#edd400"
let green  = Hex "#73d216"
let blue   = Hex "#3465a4"
let purple = Hex "#75507b"
let brown  = Hex "#c17d11"

(*-------------------------------------------------------------------*)
(* Light colors *)
(*-------------------------------------------------------------------*)

let lightYellow = Hex "#fce94f"
let lightOrange = Hex "#fcaf3e"
let lightBrown  = Hex "#e9b96e"
let lightGreen  = Hex "#8ae234"
let lightBlue   = Hex "#729fcf"
let lightPurple = Hex "#ad7fa8"
let lightRed    = Hex "#ef2929"

(*-------------------------------------------------------------------*)
(* Dark colors *)
(*-------------------------------------------------------------------*)

let darkYellow = Hex "#c4a000"
let darkOrange = Hex "#ce5c00"
let darkBrown  = Hex "#8f5902"
let darkGreen  = Hex "#4e9a06"
let darkBlue   = Hex "#204a87"
let darkPurple = Hex "#5c3566"
let darkRed    = Hex "#a40000"

(*-------------------------------------------------------------------*)
(* Shades of grey *)
(*-------------------------------------------------------------------*)

let lightGray = Hex "#eeeeec"
let gray      = Hex "#d3d7cf"
let darkGray  = Hex "#babdb6"

let lightCharcoal = Hex "#888a85"
let charcoal      = Hex "#555753"
let darkCharcoal  = Hex "#2e3436"

(*-------------------------------------------------------------------*)
(* Rainbow *)
(*-------------------------------------------------------------------*)

let rainbow = [ red; orange; yellow; green; blue; purple ]
