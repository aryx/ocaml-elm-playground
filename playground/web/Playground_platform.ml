open Common
open Basics
open Playground
open Color
module V = Vdom

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers modules *)
(*****************************************************************************)

(* can also use Printf.printf I think *)
let _log s = 
  Js_browser.Console.log Js_browser.console (Ojs.string_to_js s)

let string_of_number x = 
  spf "%f" x

module Html = struct
(*type 'msg t = 'msg V.vdom*)
let style = V.style

(*
let div a b =
  V.div ~a b
*)
end


module Svg = struct
type 'msg t = 'msg V.vdom

let svg attrs xs = 
  V.svg_elt "svg" ~a:attrs xs

let trusted_node s attrs xs = 
  V.svg_elt s ~a:attrs xs

(* !subtle! need to eta-expand. You can't factorize with
 * let circle = trusted_node "circle" otherwise
 * we don't get the general 'msg vdom type inferred but
 * the first call to circle in this file will bind forever the type
 * parameter (e.g., to `Resize of int * int).
 * You can see the wrongly inferred type by using ocamlc -i on
 * this file (you may need dune --verbose to get the full list of -I first).
 *)
let circle a b  =
  trusted_node "circle" a b
let ellipse a b =
  trusted_node "ellipse" a b
let rect a b =
  trusted_node "rect" a b
let polygon a b =
  trusted_node "polygon" a b

module Attributes = struct
let viewBox = V.attr "viewBox"

let width = V.attr "width"
let height = V.attr "height"

let r = V.attr "r"
let rx = V.attr "rx"
let ry = V.attr "ry"
let fill = V.attr "fill"
let points = V.attr "points"
let transform = V.attr "transform"
let opacity = V.attr "opacity"

end
end

(*****************************************************************************)
(* Render *)
(*****************************************************************************)

let render_color color =
  match color with
  | Hex str -> str
  | Rgb (r,g,b) -> spf "rgb(%d,%d,%d)"  r g b
    
let render_transform x y a s =
  if a = 0. then
    if s = 1.
    then
      spf "translate(%s, %s)" 
        (string_of_number x) (string_of_number (-. y))
    else
      spf "translate(%s, %s) scale(%s)" 
        (string_of_number x) (string_of_number (-. y))
        (string_of_number s)
 else
  if s = 1.
  then
      spf "translate(%s, %s) rotate(%s)" 
        (string_of_number x) (string_of_number (-. y))
        (string_of_number (-. a))
  else
      spf "translate(%s, %s) rotate(%s) scale(%s) " 
        (string_of_number x) (string_of_number (-. y))
        (string_of_number (-. a))
        (string_of_number s)

let render_rect_transform width height x y angle s =
  render_transform x y angle s ^
  spf " translate(%s, %s)" 
     (string_of_number (-. width / 2.))
     (string_of_number (-. height / 2.))


let render_alpha alpha =
  if alpha = 1.
  then []
  else [Svg.Attributes.opacity (string_of_number (clamp 0. 1. alpha))]

let render_circle color radius x y angle s alpha =
  Svg.circle 
    (Svg.Attributes.r (string_of_number radius) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_oval color width height x y angle s alpha = 
  Svg.ellipse
    (Svg.Attributes.rx (string_of_number (width / 2.)) ::
     Svg.Attributes.ry (string_of_number (height / 2.)) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_rectangle color w h x y angle s alpha = 
  Svg.rect
    (Svg.Attributes.width (string_of_number (w)) ::
     Svg.Attributes.height (string_of_number (h)) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_rect_transform w h x y angle s)::
     render_alpha alpha
    )
    []

let rec to_ngon_points i n radius str =
  if i == n 
  then str
  else
    let a = turns (float_of_int i / float_of_int n - 0.25) in
    let x = radius * cos a in
    let y = radius * sin a in
    to_ngon_points (Stdlib.(+) i 1) n radius
      (spf "%s%s,%s " str (string_of_number x) (string_of_number y))

let render_ngon color n radius x y angle s alpha = 
  Svg.polygon
    (Svg.Attributes.points (to_ngon_points 0 n radius "") ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_words _color _str _x _y _angle _s _alpha =
  raise Todo

let render_polygon _color _points _x _y _angle _s _alpha =
  raise Todo

let (render_shape: shape -> 'msg Svg.t) = 
  fun { x; y; angle; scale; alpha; form} ->
  match form with
  | Circle (color, radius) -> 
     render_circle color radius x y angle scale alpha
  | Oval (color, width, height) ->
     render_oval color width height x y angle scale alpha
  | Rectangle (color, width, height) ->
     render_rectangle color width height x y angle scale alpha
  | Ngon (color, n, radius) ->
     render_ngon color n radius x y angle scale alpha
  | Polygon (color, points) -> 
     render_polygon color points x y angle scale alpha
  | Words (color, str) ->
     render_words color str x y angle scale alpha
  | Image (_w, _h, _src) ->
      raise Todo
  | Group _ -> 
      raise Todo


let (render: screen -> shape list -> 'msg Svg.t) = fun screen shapes ->
    let w = screen.width |> string_of_number in
    let h = screen.height |> string_of_number  in
    let x = screen.left |> string_of_number  in
    let y = screen.bottom |> string_of_number in

    Svg.svg
      [Svg.Attributes.viewBox (x ^ " " ^ y ^ " " ^ w ^ " " ^ h);
       Html.style "position" "fixed";
       Html.style "top" "0";
       Html.style "left" "0";
       Svg.Attributes.width "100%";
       Svg.Attributes.height "100%";
      ]
      (List.map render_shape shapes)

(*****************************************************************************)
(* Event to msg *)
(*****************************************************************************)
(*
    | MouseMove (x, y) ->
        let x = computer.screen.left + page_x in
        let y = computer.screen.top - page_y in

let string_of_intkey = function
  | _ -> "TODO"

      (* TODO: should use subscription instead *)
    let elt = Html.div [
        V.onmousemove (fun evt -> 
          MouseMove (evt.V.page_x, evt.V.page_y)
        );
        V.onclick (fun _evt -> 
          log "click"; 
          MouseClick
        );
        (* note that Vdom does not provide onmouseup, so we use onclick 
         * to set back mouse.mdown to false *)
        V.onmousedown (fun evt -> 
          log (spf "%d" evt.V.buttons);
          MouseButton (evt.V.buttons > 0)
        );
        V.onkeydown (fun evt -> 
          log (spf "key: %d" evt.V.which);
          KeyChanged (true, string_of_intkey evt.V.which)
        );
      ] [elt] 
    in
    { Browser.
      title = "Playground";
      body = [elt];
    }

*)

(*****************************************************************************)
(* run_app *)
(*****************************************************************************)

let (vdom_app_of_app: ('model, 'msg) Playground.app -> ('model, 'msg) V.app) = 
 fun { Playground. init; view; update; subscriptions = _subTODO } ->
  V.app 
      ~init:(
      let (model, _cmdsTODO) = init () in
      model, V.Cmd.Batch [])
      ~view:(fun model ->
        (* TODO: can change! *)
        let screen = Playground.to_screen 600. 600. in
        let shapes = view model in
        render screen shapes
      )
      ~update:(fun model msg -> 
        let model, _cmds = update msg model in
        model, V.Cmd.Batch []
       )
     ()

open Js_browser
let run_app app =
  let app = vdom_app_of_app app in
  let run () = 
    Vdom_blit.run app 
    |> Vdom_blit.dom 
    |> Element.append_child (Document.body document) in
  let () = Window.set_onload window run in
  ()
