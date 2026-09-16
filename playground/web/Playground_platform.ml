open Basics
open Playground
open Color
module E = Sub

open Js_browser

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Web backend of Playground.
 *
 * TODO: switch to Canvas, use CanvasToCairo? so closer to native playground?
 * I originally used SVG because that's what elm-playground is using, but
 * I have pbs with the mouse coordinate and the bounding box conversion,
 * so maybe simpler to switch to Canvas.
 *)

(* When set to true, we generate a new frame only when there is an event.
 * It makes things easier to observe with Chrome developer tools.
 *)
let debug = ref false

(*****************************************************************************)
(* Helpers modules *)
(*****************************************************************************)

(* can also use Printf.printf I think *)
let log s = 
  Js_browser.Console.log Js_browser.console (Ojs.string_to_js s)

let spf = Printf.sprintf

let string_of_number x = 
  spf "%f" x

(* when using the VDROM, but tedious to use request_animation_frame
 * and a global keyboard handler => switch to basic Dom.
 *)
(*
module V = Vdom
*)

(* when using directly the DOM *)
module V = struct

type attr = 
  | Attr of string * string
  | Style of string * string

(* claude: a tiny virtual DOM. We used to build real DOM elements directly
 * here and replace the whole <svg> on every animation frame, but a freshly
 * created <svg:image> decodes its picture asynchronously (even when the
 * url is in the browser cache), so some frames were painted without the
 * sprite (Mario "disappearing"), and animated GIFs restarted every frame.
 * Now [render] returns this description, and [patch] below updates the
 * existing DOM in place, touching only the attributes that changed.
 *)
type t = {
  tag: string;
  attrs: attr list;
  children: t list;
}

type 'a vdom = t

let svg_ns = "http://www.w3.org/2000/svg"

let svg_elt tag ~a children =
  { tag; attrs = a; children }

let style s1 s2  = 
  Style (s1, s2)
let attr s v =
  Attr (s, v)

let set_attr elt = function
  | Attr (k, v) ->
      Element.set_attribute elt k v
  | Style (k, v) ->
      Ojs.set_prop_ascii
        (Ojs.get_prop_ascii (Element.t_to_js elt) "style")
        k
        (Ojs.string_to_js v)

let remove_attr elt = function
  | Attr (k, _) -> Element.remove_attribute elt k
  | Style (k, _) -> set_attr elt (Style (k, ""))

let same_key a b =
  match a, b with
  | Attr (k1, _), Attr (k2, _) | Style (k1, _), Style (k2, _) -> k1 = k2
  | _ -> false

let rec create (node : t) : Element.t =
  (* bugfix: for svg elt we need to pass the ns! (namespace_URI), otherwise
   * it will not render anything.
   *)
  let elt = Document.create_element_ns document svg_ns node.tag in
  node.attrs |> List.iter (set_attr elt);
  node.children |> List.iter (fun child ->
      Element.append_child elt (create child)
  );
  elt

(* [elt] is the DOM element previously created (or patched) from [old];
 * returns the DOM element now corresponding to [node]. *)
let rec patch ~(parent : Element.t) (elt : Element.t) (old : t) (node : t)
    : Element.t =
  if old.tag <> node.tag then begin
    let fresh = create node in
    Element.replace_child parent fresh elt;
    fresh
  end else begin
    old.attrs |> List.iter (fun a ->
      if not (List.exists (same_key a) node.attrs)
      then remove_attr elt a
    );
    node.attrs |> List.iter (fun a ->
      (* only touch changed attributes; re-setting an <image> href
       * could trigger a reload *)
      if not (List.mem a old.attrs)
      then set_attr elt a
    );
    patch_children elt (Element.first_child elt) old.children node.children;
    elt
  end

and patch_children parent child olds nodes =
  match olds, nodes with
  | [], [] -> ()
  | o :: olds, n :: nodes ->
      let next = Element.next_sibling child in
      ignore (patch ~parent child o n);
      patch_children parent next olds nodes
  | [], n :: nodes ->
      Element.append_child parent (create n);
      patch_children parent child [] nodes
  | _ :: olds, [] ->
      let next = Element.next_sibling child in
      Element.remove_child parent child;
      patch_children parent next olds []
end

module Html = struct
let style = V.style
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
let image a b =
  trusted_node "image" a b

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

let href = V.attr "href"

end
end

(*****************************************************************************)
(* Render *)
(*****************************************************************************)

let render_color color =
  match color with
  | Hex str -> str
  | Rgb (r,g,b) -> Printf.sprintf "rgb(%d,%d,%d)"  r g b
    
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

(* TODO *)
let render_words color _str x y angle s alpha =
  Svg.circle 
    (Svg.Attributes.r (string_of_number 10.) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

(* TODO *)
let render_polygon color _points x y angle s alpha =
  Svg.circle 
    (Svg.Attributes.r (string_of_number 10.) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_image w h src x y angle s alpha =
  Svg.image
    (Svg.Attributes.href src:: (* was xlinkHref but require attributeNS *)
     Svg.Attributes.width (string_of_number w)::
     (* claude: was a second 'width', so 'height' was never set *)
     Svg.Attributes.height (string_of_number h)::
     Svg.Attributes.fill (render_color yellow) ::
     Svg.Attributes.transform (render_rect_transform w h x y angle s)::
     render_alpha alpha
    )
    []


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
  | Image (w, h, src) ->
     render_image w h src x y angle scale alpha
  | Group _ -> 
      failwith "Todo"


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
(* Event management *)
(*****************************************************************************)

let adjust_x_y x y dim screen =
  log (spf "%f %f" x y);
  log (spf "h=%f, w=%f, left=%f, right=%f, top=%f, bottom=%f"
        (Rect.height dim) (Rect.width dim) (Rect.left dim) (Rect.right dim)
        (Rect.top dim) (Rect.bottom dim));

  let x = x - Rect.left dim in
  let x = x * screen.width / Rect.width dim in
  let x = screen.left + x in

  let y = y - Rect.top dim in
  let y = y * screen.height / Rect.height dim in
  let y = screen.top - y in

   x, y

let adjust_key key = 
  log (spf "key = '%s'" key);
  match key with
  | " " -> "space"
  | _ -> key

let js_event_to_event evt screen = 
  let ty = Event.type_ evt in
  match ty with
  | "mousemove" ->
      let (x, y) = Event.page_x evt, Event.page_y evt in
  
      let o = Event.target evt in
      let elt = Element.t_of_js o in
      let dim = Element.get_bounding_client_rect elt in

      let x, y = adjust_x_y x y dim screen in
      Some (E.EMouseMove (int_of_float x, int_of_float y))
  | "mousedown" | "mouseup" ->
      let b = Event.buttons evt > 0 in
      Some (E.EMouseButton b)

  | "keydown" ->
      let key = Event.key evt in
      let key = adjust_key key in
      Some (E.EKeyChanged (true, key))
  | "keyup" ->
      let key = Event.key evt in
      let key = adjust_key key in
      Some (E.EKeyChanged (false, key))

  | _ -> None

(*****************************************************************************)
(* run_app *)
(*****************************************************************************)

(* when using Vdom:
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
let run_app app =
  let app = vdom_app_of_app app in
  let run () = 
    Vdom_blit.run app 
    |> Vdom_blit.dom 
    |> Element.append_child (Document.body document) in
  let () = Window.set_onload window run in
  ()
*)

(* claude: start downloading (and decoding) the image right away, and
 * keep a reference to the Image object so the browser keeps it in its
 * memory cache; this way an <svg:image> switching to this url later
 * (e.g., Mario going from "walk" to "jump") can paint it without waiting
 * for the network.
 *)
let preloaded : (string, Ojs.t) Hashtbl.t = Hashtbl.create 16

let preload_image (url : string) =
  if not (Hashtbl.mem preloaded url) then begin
    let img = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Image") [||] in
    Ojs.set_prop_ascii img "src" (Ojs.string_to_js url);
    Hashtbl.replace preloaded url img
  end

(* when using the simple DOM *)
let run_app app =
  Window.set_onload window (fun () ->

    let sx = Playground.default_width in
    let sy = Playground.default_height in
    let screen = Playground.to_screen sx sy in

    let (initmodel, _cmdsTODO) = app.Playground.init () in
    let model = ref initmodel in

    let process_playground_event event = 
      let subs = app.Playground.subscriptions !model in
      let msg_opt = E.event_to_msgopt event subs in
      (match msg_opt with
      | None -> ()
      | Some msg ->
          let newmodel, _cmds = app.Playground.update msg !model in
          model := newmodel;
     );
    in
   
    (* claude: requestAnimationFrame fires at the display refresh rate,
     * which is 120Hz (or more) on many screens (e.g., Mac ProMotion), but
     * Playground.game's update functions (e.g., examples/Mario.ml) use a
     * fixed per-tick dt assuming ~60Hz, so delivering one Tick per
     * animation frame made games run 2x too fast there. We now deliver
     * Ticks on a fixed 60Hz timestep, independent of the refresh rate
     * (same fix as the frame cap in playground/native/).
     *)
    let tick_period = 1. /. 60. in
    (* tolerate jitter in rAF timestamps on 60Hz displays, otherwise
     * we would sometimes skip a Tick and then do 2 in the next frame *)
    let tick_slack = 0.002 in
    let last_time = ref None in
    let pending = ref 0. in

    (* measured rAF rate, logged once, to help diagnose timing issues *)
    let rate_start = ref None in
    let rate_frames = ref 0 in

    (* the current <svg> and the vdom it was built from *)
    let current = ref None in

    (* one frame *)
    let rec animation_frame time =
      let time = time /. 1000. in

      (match !rate_start with
      | None -> rate_start := Some time
      | Some start ->
          incr rate_frames;
          if !rate_frames = 120 then
            log (spf "requestAnimationFrame rate: %.0f Hz"
                   (float_of_int !rate_frames /. (time -. start)))
      );

      (match !last_time with
      | None -> pending := tick_period
      | Some last -> pending := !pending +. (time -. last)
      );
      last_time := Some time;
      (* after a long pause (e.g., the tab was hidden) don't try to catch up *)
      if !pending > 0.25 then pending := tick_period;
      while !pending >= tick_period -. tick_slack do
        process_playground_event (E.ETick time);
        pending := !pending -. tick_period
      done;

      let shapes = app.Playground.view !model in
      let node = render screen shapes in
      let body = Document.body document in
      (match !current with
      | None ->
          Element.remove_all_children body;
          let elt = V.create node in
          Element.append_child body elt;
          current := Some (node, elt)
      | Some (old, elt) ->
          let elt = V.patch ~parent:body elt old node in
          current := Some (node, elt)
      );

      if not !debug 
      then Window.request_animation_frame window animation_frame;
    in
    Window.request_animation_frame window animation_frame;

    let on_js_event evt =
      let evt_opt = js_event_to_event evt screen in
      (match evt_opt with
      | None -> ()
      | Some event -> process_playground_event event
      );
      if !debug then Window.request_animation_frame window animation_frame;
    in
    [
      Event.Mousemove;
      Event.Mousedown;
      Event.Mouseup;
      Event.Keydown;
      Event.Keyup;
    ] |> List.iter (fun evt_kind ->
       Window.add_event_listener window evt_kind on_js_event true
    );
  )
