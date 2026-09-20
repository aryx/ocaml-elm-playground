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
 * history: ocaml-vdom's Vdom vs our hand-made mini virtual DOM
 * ---------------------------------------------------------------------
 * This file uses the "vdom" opam package (LexiFi's ocaml-vdom), but only
 * its Js_browser module (typed OCaml bindings to the browser API:
 * Window, Document, Element, Event, Date, ...), not its Vdom module.
 *
 * 1) First version (2020): I originally wrote a real Vdom app (see the
 *    commented vdom_app_of_app/run_app near the end of this file). Vdom works
 *    like Elm: you give it view/update functions, it owns the main loop, and
 *    after each update it diffs the new virtual DOM with the previous
 *    one and patches the real DOM. But two things playground needs were
 *    missing, which I asked about in the ocaml-vdom GitHub issues
 *    at https://github.com/LexiFi/ocaml-vdom/issues/ :
 *    - #35: no canvas in Vdom; the answer was to use SVG, which we do.
 *    - #37: no way to get a Tick message on each animation frame
 *      (Elm's onAnimationFrame subscription);
 *    - #36: onkeydown on a <div> only fires when that div (or a child)
 *      has the keyboard focus, i.e., only after clicking in the page,
 *      whereas games need to see all key presses (and key releases);
 *    The maintainers' answer for #36/#37 was to use "custom elements":
 *    Vdom's escape hatch (a Vdom.custom node in the view, plus a
 *    handler registered with Vdom_blit.register (Vdom_blit.custom ...))
 *    to include in the view an element managed by your own
 *    imperative code; that code, run when the element is created, could
 *    start a requestAnimationFrame loop, or install key listeners on
 *    window (which receives key presses whatever has the focus), and
 *    send the results as messages to the Vdom app.
 *    But this originally looked complicated to me and I didn't fully understand.
 *
 * 2) Current version: instead of plugging our own code into Vdom's
 *    main loop via custom elements, run_app below *is* the main loop, so
 *    it does directly what those custom elements would have done:
 *    - animation_frame re-registers itself with
 *      Window.request_animation_frame and sends ETick to update (#37);
 *    - Window.add_event_listener window Keydown/Keyup/Mouse... receives
 *      all key and mouse events, whatever has the focus (#36).
 *    What we lost by leaving Vdom is its diffing: the first direct-DOM
 *    version rebuilt the whole <svg> at each frame, which was simple but
 *    made <image> sprites disappear now and then (a new <image> element
 *    decodes its picture asynchronously) and restarted animated GIFs.
 *    Claude then adjusted the module V below with a tiny hand-made virtual
 *    DOM (~100 lines) with its own diff/patch (V.patch), specialized for
 *    our case (flat lists of SVG shapes, children matched by position)
 *    see https://github.com/aryx/ocaml-elm-playground/commit/e97df45
 *
 * If one day playground needs to be embedded in a bigger Vdom page
 * (e.g., with buttons or text inputs around the game), going back to
 * Vdom with custom elements as described in 1) would make sense. For a
 * standalone full-page playground, the direct approach is simpler.
 *
 * TODO:
 *  - still? switch to Canvas, use CanvasToCairo? so closer to native playground?
 *    I originally used SVG because that's what elm-playground is using, but
 *    I had pbs with the mouse coordinate and the bounding box conversion,
 *    so was maybe simpler to switch to Canvas?
 *    claude: the mouse coordinate problem is fixed now (see adjust_x_y),
 *    which removes one reason to switch to Canvas.
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* When set to true, we generate a new frame only when there is an event.
 * It makes things easier to observe with Chrome developer tools.
 *)
let debug = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* can also use Printf.printf I think *)
let log s = 
  Js_browser.Console.log Js_browser.console (Ojs.string_to_js s)

let spf = Printf.sprintf

let string_of_number x = 
  spf "%f" x

(*****************************************************************************)
(* (Mini) Virtual DOM *)
(*****************************************************************************)

(* alt: when using the VDOM, but tedious to use request_animation_frame
 * and a global keyboard handler => switch to basic Dom (and mini-vdom later)
 *
 * module V = Vdom
 *)

(* when using directly the DOM *)
module V = struct

type attr = 
  | Attr of string * string
  | Style of string * string
  (* claude: a JavaScript property of the element object rather than an
   * attribute in the markup; used for "textContent", the text inside an
   * element (e.g., the string displayed by <text>) *)
  | Prop of string * string

(* claude: a tiny "virtual DOM".
 *
 * Background: the DOM (Document Object Model) is the tree of live
 * elements the browser displays (<body>, <svg>, <circle>, <image>, ...).
 * The browser redraws the screen from this tree after each
 * animation frame. A "virtual DOM" is just a plain data structure
 * *describing* such a tree (like the [t] type below); it costs nothing
 * to build a new one each frame, and it is not displayed. Libraries like
 * Elm or ocaml-vdom then compare ("diff") the new description with the
 * previous one and apply only the differences to the real DOM
 * ("patching").
 *
 * The problem with the old code: this module used to build *real* DOM
 * elements directly (type t = Element.t), and run_app removed the whole
 * <svg> and inserted a brand-new one on every frame (60+ times per
 * second). That looked fine for circles and rectangles, but not for
 * images: a newly created <image> element loads and decodes its picture
 * asynchronously, even when the url is already in the browser cache, so
 * the browser sometimes displayed a frame before the picture was ready
 * -> the Mario sprite disappeared for a frame now and then. It also
 * restarted animated GIFs (Mario's "walk" sprite) at their first frame
 * each time, so they never animated.
 *
 * The new code: [render] (and the Svg helpers below) now return a [t]
 * value, i.e., a description, and [patch] updates the previous frame's
 * real elements in place. Frame after frame, Mario's <image> is the
 * *same* DOM element, and its href attribute is only modified when the
 * sprite really changes (e.g., from "walk" to "jump").
 *
 * old: alt: direct DOM with 'type t = Element.t
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
let prop s v =
  Prop (s, v)

(* Set one attribute (e.g., <circle r="10">) or one CSS style property
 * (e.g., style="position: fixed") on a real DOM element.
 * For styles, js_browser has no binding, so we use Ojs, the low-level
 * js_of_ocaml/gen_js_api module to manipulate raw JavaScript values:
 * the code below is the OCaml version of the JavaScript
 *   elt.style[k] = v
 * (Element.t_to_js converts the typed OCaml value to a raw JS value,
 * get_prop_ascii/set_prop_ascii read/write a JS object field, and
 * string_to_js converts an OCaml string into a JS string).
 *)
let set_attr elt = function
  | Attr (k, v) ->
      Element.set_attribute elt k v
  | Style (k, v) ->
      Ojs.set_prop_ascii
        (Ojs.get_prop_ascii (Element.t_to_js elt) "style")
        k
        (Ojs.string_to_js v)
  | Prop (k, v) ->
      (* elt[k] = v *)
      Ojs.set_prop_ascii (Element.t_to_js elt) k (Ojs.string_to_js v)

(* Undo set_attr (setting a style property or textContent to "" removes
 * it). *)
let remove_attr elt = function
  | Attr (k, _) -> Element.remove_attribute elt k
  | Style (k, _) -> set_attr elt (Style (k, ""))
  | Prop (k, _) -> set_attr elt (Prop (k, ""))

(* Do the two attributes set the same thing (regardless of the value)?
 * e.g., Attr ("r", "10") and Attr ("r", "20") *)
let same_key a b =
  match a, b with
  | Attr (k1, _), Attr (k2, _)
  | Style (k1, _), Style (k2, _)
  | Prop (k1, _), Prop (k2, _) -> k1 = k2
  | _ -> false

(* Build real DOM elements from a description; used for the first frame
 * and for parts of the tree that did not exist in the previous frame. *)
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

(* Make the real element [elt], which currently displays the description
 * [old] (the previous frame), display [node] (the new frame) instead,
 * doing as few DOM modifications as possible. [parent] is [elt]'s
 * parent in the DOM, needed only if we must replace [elt] entirely.
 * Returns the real element now displaying [node] ([elt] itself, unless
 * it was replaced).
 *
 * This is a simplified version of what Elm/ocaml-vdom do: children
 * are matched by position (the 1st child of old with the 1st child of
 * node, etc.), which is fine for playground since view functions
 * usually return the same list of shapes in the same order every frame
 * with just different positions/colors.
 *)
let rec patch ~(parent : Element.t) (elt : Element.t) (old : t) (node : t)
    : Element.t =
  (* different kind of element (e.g., a <circle> became a <rect>):
   * cannot modify it in place, build a new one *)
  if old.tag <> node.tag then begin
    let fresh = create node in
    Element.replace_child parent fresh elt;
    fresh
  end else begin
    (* same kind of element: 1) remove attributes no longer present
     * (e.g., opacity when a shape stops being faded) *)
    old.attrs |> List.iter (fun a ->
      if not (List.exists (same_key a) node.attrs)
      then remove_attr elt a
    );
    node.attrs |> List.iter (fun a ->
      (* 2) set only new or changed attributes (List.mem compares
       * both the name and the value); in particular we don't re-set an
       * unchanged <image> href, which could make the browser reload it *)
      if not (List.mem a old.attrs)
      then set_attr elt a
    );
    (* 3) same thing recursively for the children *)
    patch_children elt (Element.first_child elt) old.children node.children;
    elt
  end

(* Walk in parallel the old children descriptions [olds], the new ones
 * [nodes], and the real children of [parent] (starting at [child]; the
 * real children correspond 1-to-1 to [olds] since we built them).
 * Element.first_child/next_sibling are the DOM way to iterate over the
 * children of an element (next_sibling = the next child of the same
 * parent). *)
and patch_children parent child olds nodes =
  match olds, nodes with
  | [], [] -> ()
  (* a child in both frames: patch it *)
  | o :: olds, n :: nodes ->
      (* get the next sibling before patching, in case child is replaced *)
      let next = Element.next_sibling child in
      ignore (patch ~parent child o n);
      patch_children parent next olds nodes
  (* more shapes than in the previous frame: add them at the end *)
  | [], n :: nodes ->
      Element.append_child parent (create n);
      patch_children parent child [] nodes
  (* fewer shapes than in the previous frame: remove the extra ones *)
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
let text_ a b =
  trusted_node "text" a b
(* <g> (for "group") has no drawing of its own, it applies its attributes
 * (e.g., transform, opacity) to all its children *)
let g a b =
  trusted_node "g" a b

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

let textAnchor = V.attr "text-anchor"
let dominantBaseline = V.attr "dominant-baseline"
let fontSize = V.attr "font-size"
let fontFamily = V.attr "font-family"
(* claude: not an attribute in Elm's Svg module (Elm uses a text child
 * node instead), but simpler with our tiny virtual DOM *)
let textContent = V.prop "textContent"

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

(* claude: same as renderWords in elm-playground: the text is centered on (x, y)
 * horizontally (text-anchor) and vertically (dominant-baseline). *)
let render_words color str x y angle s alpha =
  Svg.text_
    (Svg.Attributes.textAnchor "middle" ::
     Svg.Attributes.dominantBaseline "central" ::
     (* claude: same font as the native backend, instead of the browser
      * default (see Playground.words_font_size) *)
     Svg.Attributes.fontSize (string_of_number words_font_size) ::
     Svg.Attributes.fontFamily words_font_family ::
     Svg.Attributes.textContent str ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

(* claude: Same as renderPolygon in elm-playground: the points are relative to
 * (x, y), and their y is negated since the svg y axis goes down (see
 * render_transform). *)
let render_polygon color points x y angle s alpha =
  let points_str =
    points
    |> List.map (fun (px, py) ->
        spf "%s,%s" (string_of_number px) (string_of_number (-. py)))
    |> String.concat " "
  in
  Svg.polygon
    (Svg.Attributes.points points_str ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_image w h src x y angle s alpha =
  Svg.image
    (Svg.Attributes.href src:: (* was xlinkHref but require attributeNS *)
     Svg.Attributes.width (string_of_number w)::
     Svg.Attributes.height (string_of_number h)::
     Svg.Attributes.fill (render_color yellow) ::
     Svg.Attributes.transform (render_rect_transform w h x y angle s)::
     render_alpha alpha
    )
    []


let rec (render_shape: shape -> 'msg Svg.t) = 
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
  (* claude: was a failwith "Todo". Same as renderGroup in
   * elm-playground: an svg <g> whose transform and opacity apply to all
   * the shapes in the group, which are positioned relative to the group
   * (x, y). *)
  | Group shapes ->
      Svg.g
        (Svg.Attributes.transform (render_transform x y angle scale) ::
         render_alpha alpha
        )
        (List.map render_shape shapes)


let (render: rendering:rendering -> screen -> shape list -> 'msg Svg.t) =
 fun ~rendering screen shapes ->
    let w = screen.width |> string_of_number in
    let h = screen.height |> string_of_number  in
    let x = screen.left |> string_of_number  in
    let y = screen.bottom |> string_of_number in

    Svg.svg
      ([Svg.Attributes.viewBox (x ^ " " ^ y ^ " " ^ w ^ " " ^ h);
       Html.style "position" "fixed";
       Html.style "top" "0";
       Html.style "left" "0";
       Svg.Attributes.width "100%";
       Svg.Attributes.height "100%";
      ] @
      (* claude: Playground.rendering, the browser's own switches; both
       * are inherited by all the shapes inside the <svg> *)
      (if rendering.antialiasing then [] else [V.attr "shape-rendering" "crispEdges"]) @
      (if rendering.smooth_images then [] else [Html.style "image-rendering" "pixelated"]))
      (List.map render_shape shapes)

(*****************************************************************************)
(* Event management *)
(*****************************************************************************)

(* claude: convert the mouse position of a JavaScript mouse event into
 * playground coordinates.
 *
 * There are 2 coordinate systems involved:
 *  - "client" coordinates, which the browser gives us in mouse events
 *    (Event.client_x/client_y): pixels from the top-left corner of the
 *    browser window, y going down.
 *  - the <svg> "user" coordinates, the ones we draw in, set by the
 *    viewBox attribute in [render]: here x from -500 (left) to 500
 *    (right), y from -500 (top) to 500 (bottom), since render_transform
 *    negates y. The <svg> is stretched to fill the whole window
 *    (width/height 100%), but by default (preserveAspectRatio) the
 *    browser keeps the drawing square and centered, so if the window is
 *    wider than tall there are empty bands on the left and right (or at
 *    the top/bottom otherwise).
 *
 * The problem with the old code: it computed the scaling from the
 * bounding box (position and size on the page) of Event.target, which
 * is the *element under the mouse pointer*, not necessarily the <svg>.
 * In examples/Mouse.ml, most of the time the pointer is over the big
 * yellow rectangle, so the math was roughly right; but as soon as the
 * purple circle reached the pointer, the target became the circle,
 * whose bounding box is small and elsewhere, so the computed position
 * was wrong, the circle moved away, the next event was again over the
 * rectangle, the circle moved back, etc. -> flickering and wrong
 * positions. It also ignored the empty bands mentioned above.
 *
 * The new code: always use the root <svg> element, and ask the browser
 * itself for the conversion. svg.getScreenCTM() returns the matrix
 * converting svg user coordinates to client coordinates (taking into
 * account the viewBox, the window size, the empty bands, the scrolling,
 * ...); its inverse converts the other way, which is what we need.
 * js_browser has no binding for those SVG functions, so we call them
 * via Ojs (see set_attr above); the code is the OCaml version of
 * this JavaScript:
 *
 *   let pt = svg.createSVGPoint();   // a {x, y} point object
 *   pt.x = client_x; pt.y = client_y;
 *   pt = pt.matrixTransform(svg.getScreenCTM().inverse());
 *   return [pt.x, -pt.y];
 *
 * (Ojs.call obj "meth" [|args|] is obj.meth(args), and
 * Ojs.float_to_js/float_of_js convert between OCaml and JS numbers.)
 *)
let adjust_x_y (svg : Element.t) (client_x : float) (client_y : float) =
  let svg = Element.t_to_js svg in
  let pt = Ojs.call svg "createSVGPoint" [||] in
  Ojs.set_prop_ascii pt "x" (Ojs.float_to_js client_x);
  Ojs.set_prop_ascii pt "y" (Ojs.float_to_js client_y);
  let ctm = Ojs.call svg "getScreenCTM" [||] in
  let inv = Ojs.call ctm "inverse" [||] in
  let pt = Ojs.call pt "matrixTransform" [| inv |] in
  let x = Ojs.float_of_js (Ojs.get_prop_ascii pt "x") in
  let y = Ojs.float_of_js (Ojs.get_prop_ascii pt "y") in
  (* the svg y axis goes down but the playground one goes up
   * (see render_transform) *)
  x, -. y

let adjust_key key = 
  log (spf "key = '%s'" key);
  match key with
  | " " -> "space"
  | _ -> key

let js_event_to_event evt (svg_opt : Element.t option) = 
  let ty = Event.type_ evt in
  match ty, svg_opt with
  | "mousemove", None -> None
  | "mousemove", Some svg ->
      let x, y = adjust_x_y svg (Event.client_x evt) (Event.client_y evt) in
      Some (E.EMouseMove (int_of_float x, int_of_float y))
  (* claude: [button] (not in vdom's binding, hence Ojs) is the button
   * that changed: 0 the left (main) one, 2 the right one; [buttons] is
   * a bitmask of those still held, 1 for the left one *)
  | "mousedown", _ when Ojs.int_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) "button") = 2 ->
      Some (E.ERightMouseButton true)
  | "mouseup", _ when Ojs.int_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) "button") = 2 ->
      Some (E.ERightMouseButton false)
  | ("mousedown" | "mouseup"), _ ->
      let b = Event.buttons evt land 1 <> 0 in
      Some (E.EMouseButton b)

  | "keydown", _ ->
      let key = Event.key evt in
      let key = adjust_key key in
      Some (E.EKeyChanged (true, key))
  | "keyup", _ ->
      let key = Event.key evt in
      let key = adjust_key key in
      Some (E.EKeyChanged (false, key))

  (* claude: the wheel and the double click, for applications rather
   * than games (plan_gui_teaching.md, phase 0). The browser's deltaY
   * is in pixels, lines or pages (deltaMode) and grows downwards,
   * where the playground's mwheel is notches and grows upwards, so
   * normalize: a notch is about 100 pixels or 3 lines. *)
  | "wheel", _ ->
      let get prop = Ojs.float_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) prop) in
      let delta = get "deltaY" in
      let mode = int_of_float (get "deltaMode") in
      let notches = match mode with 0 -> delta /. 100. | 1 -> delta /. 3. | _ -> delta in
      Some (E.EMouseWheel (-. notches))
  | "dblclick", _ -> Some E.EMouseDouble

  | _ -> None

(* claude: is this keydown's [key] a character the person typed, rather
 * than a named key? The browser gives the character itself for
 * character keys ("a", "A" with shift, "e" with an accent from a dead
 * key, whatever a layout puts there) and an ASCII word otherwise
 * ("Shift", "ArrowUp", "Backspace", "F1"), so: one byte, or a
 * non-ASCII first byte (an accented character is several UTF-8 bytes).
 * No IME support, which is out of this plan's scope and said so. *)
let typed_of_key (key : string) : string option =
  if key = "" then None
  else if String.length key = 1 || Char.code key.[0] >= 0x80 then Some key
  else None

(*****************************************************************************)
(* run_app *)
(*****************************************************************************)

(* alt: when using Vdom (but Tick and Key issues, see notes about it before):
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
 * for the network. The old code did nothing here, so the first jump
 * could show no sprite until the download was done.
 *
 * The Ojs code below is the OCaml version of the JavaScript
 *   let img = new Image(); img.src = url;
 * (Ojs.global is the JS global object, where the Image class lives, and
 * Ojs.new_obj calls a JS constructor.) Setting src is what starts the
 * download; the image is not inserted in the page. We store img in
 * [preloaded] so it is not garbage collected (which could evict it
 * from the browser memory cache).
 *)
let preloaded : (string, Ojs.t) Hashtbl.t = Hashtbl.create 16

let preload_image (url : string) =
  if not (Hashtbl.mem preloaded url) then begin
    let img = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Image") [||] in
    Ojs.set_prop_ascii img "src" (Ojs.string_to_js url);
    Hashtbl.replace preloaded url img
  end

(* claude: the page's URL parameters, "?level=5&fast" ->
 * [("level", "5"); ("fast", "")], the web's command line (see
 * Playground.flags). The Ojs code is the JavaScript
 * window.location.search (vdom's Location has no binding for it). No
 * %-decoding: flags are meant to be short names and values. *)
let flags () : Playground.flags =
  let search = Ojs.string_of_js (Ojs.get_prop_ascii (Ojs.get_prop_ascii Ojs.global "location") "search") in
  let search =
    if String.length search > 0 && search.[0] = '?'
    then String.sub search 1 (Stdlib.(-) (String.length search) 1)
    else search
  in
  Playground.flags_of_strings (String.split_on_char '&' search)

(*****************************************************************************)
(* Sound: Web Audio *)
(*****************************************************************************)

(* claude: the same sound as natively, every sample ours (Audio.pull,
 * audio/Mixer.mli): each frame, the samples the browser's audio clock
 * will need next are copied into an AudioBuffer, scheduled right after
 * the previous one, about 100 ms ahead, so that they play back to back
 * with no gap (the Web Audio API: an AudioContext, its currentTime, a
 * buffer source started at a given time; the browser resamples our
 * 44,100 a second to its own rate). Browsers start an AudioContext
 * "suspended" until the page gets a click or a key (their autoplay
 * policy): resumed on the first input event; until then the samples are
 * pulled and dropped, so that sounds don't pile up.
 * The other way, the browser's own OscillatorNodes and GainNodes
 * computing the sound (no samples of ours), is left for comparison
 * (plan_audio_teaching.md, phase 4).
 * References: https://www.w3.org/TR/webaudio/ ;
 * https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API *)
let audio_context : Ojs.t option Lazy.t =
  lazy
    (let ctor = Ojs.get_prop_ascii Ojs.global "AudioContext" in
     if Ojs.type_of ctor = "undefined" then None else Some (Ojs.new_obj ctor [||]))

let audio_state (ctx : Ojs.t) : string = Ojs.string_of_js (Ojs.get_prop_ascii ctx "state")

let resume_audio () : unit =
  match Lazy.force audio_context with
  | Some ctx when audio_state ctx = "suspended" -> ignore (Ojs.call ctx "resume" [||])
  | _ -> ()

(* when the next buffer starts, on the AudioContext's clock *)
let next_start = ref 0.

(* after a frame's [ticks] updates *)
let play_audio (ticks : int) : unit =
  match Lazy.force audio_context with
  | Some ctx when audio_state ctx = "running" ->
      let now = Ojs.float_of_js (Ojs.get_prop_ascii ctx "currentTime") in
      (* late (the tab was hidden, or the start): start again a bit
       * ahead *)
      if !next_start < now then next_start := now +. 0.05;
      let n = int_of_float ((0.1 -. (!next_start -. now)) *. 44100.) in
      if n > 0 then (
        let samples = Audio.pull n in
        let buffer = Ojs.call ctx "createBuffer" [| Ojs.int_to_js 1; Ojs.int_to_js n; Ojs.int_to_js 44100 |] in
        let data = Ojs.call buffer "getChannelData" [| Ojs.int_to_js 0 |] in
        Array.iteri (fun i x -> Ojs.array_set data i (Ojs.float_to_js x)) samples;
        let source = Ojs.call ctx "createBufferSource" [||] in
        Ojs.set_prop_ascii source "buffer" buffer;
        ignore (Ojs.call source "connect" [| Ojs.get_prop_ascii ctx "destination" |]);
        ignore (Ojs.call source "start" [| Ojs.float_to_js !next_start |]);
        next_start := !next_start +. (float_of_int n /. 44100.))
  | _ -> ignore (Audio.pull (ticks *.. (44100 /.. 60)))

(* claude: Audio.loop_from's files, fetched in the background (an
 * XMLHttpRequest, its response as bytes): a plain name from the page's
 * own server, a URL elsewhere if that server allows it (CORS) *)
let fetch_web (source : string) (k : string option -> unit) : unit =
  let xhr = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "XMLHttpRequest") [||] in
  ignore (Ojs.call xhr "open" [| Ojs.string_to_js "GET"; Ojs.string_to_js source |]);
  Ojs.set_prop_ascii xhr "responseType" (Ojs.string_to_js "arraybuffer");
  Ojs.set_prop_ascii xhr "onload"
    (Ojs.fun_to_js 1 (fun _ ->
         let status = Ojs.int_of_js (Ojs.get_prop_ascii xhr "status") in
         if status >= 200 && status < 300 then (
           let bytes = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Uint8Array") [| Ojs.get_prop_ascii xhr "response" |] in
           let n = Ojs.int_of_js (Ojs.get_prop_ascii bytes "length") in
           k (Some (String.init n (fun i -> Char.chr (Ojs.int_of_js (Ojs.array_get bytes i))))))
         else k None));
  Ojs.set_prop_ascii xhr "onerror" (Ojs.fun_to_js 1 (fun _ -> k None));
  ignore (Ojs.call xhr "send" [||])

(* when using the simple DOM *)
let run_app ?(rendering = Playground.default_rendering) ?(flags = []) app =
  Audio.set_fetcher fetch_web;
  Window.set_onload window (fun () ->

    let sx = Playground.default_width in
    let sy = Playground.default_height in
    let screen = Playground.to_screen sx sy in

    let (initmodel, _cmdsTODO) = app.Playground.init flags in
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
   
    (* claude: game speed.
     *
     * Window.request_animation_frame window f asks the browser to call
     * f just before it next redraws the screen (f receives the current
     * time in milliseconds). animation_frame below re-registers itself
     * each time, so it is called once per screen refresh.
     *
     * The problem with the old code: it delivered one Tick per call,
     * i.e., one per screen refresh. Playground games advance by a fixed
     * amount per Tick (e.g., examples/Mario.ml's dt), tuned for 60
     * refreshes per second, but many screens refresh faster (Mac
     * ProMotion screens: 120 per second), so Mario ran 2x too fast.
     *
     * The new code: count how much time passed since the previous call
     * (in [pending]), and deliver one Tick per full 1/60s elapsed. At 120Hz
     * that's a Tick every other frame; at 60Hz one per frame; at 30Hz two
     * per frame. The screen is still redrawn every frame. This is the
     * classic "fixed timestep" game loop (same idea as the 60fps cap in
     * playground/native/).
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

      (* time elapsed since the previous frame (one Tick on the
       * very first frame) *)
      (match !last_time with
      | None -> pending := tick_period
      | Some last -> pending := !pending +. (time -. last)
      );
      last_time := Some time;
      (* after a long pause (e.g., the tab was hidden) don't try to catch up *)
      if !pending > 0.25 then pending := tick_period;
      (* claude: the Tick carries the wall-clock time (seconds since
       * 1970), not [time] (seconds since the page was loaded, which is
       * what requestAnimationFrame gives us). That's what the native
       * backend passes (Unix.gettimeofday) and what Elm's
       * onAnimationFrame passes (Time.Posix), and games rely on it:
       * games/Tetris.ml and games/Asteroid.ml initialize their last_tick
       * with Unix.gettimeofday() and compute [now -. last_tick] on each
       * Tick. With [time], that delta was about -1.8 billion seconds:
       * Tetris' piece started 1.8 billion rows above the well (and a
       * full drop with space then looped 1.8 billion times, freezing the
       * tab), and Asteroid ignored all Ticks (delta < tick) so it never
       * started. *)
      let wall_clock = Date.now () /. 1000. in
      let ticks = ref 0 in
      while !pending >= tick_period -. tick_slack do
        process_playground_event (E.ETick wall_clock);
        pending := !pending -. tick_period;
        incr ticks
      done;
      (* claude: the sounds those updates played *)
      play_audio !ticks;

      (* redraw: compute the description of the new frame, then either
       * build the real <svg> (first frame) or update the existing one
       * (see V.patch) *)
      let shapes = app.Playground.view !model in
      let node = render ~rendering screen shapes in
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
      (* claude: the browser lets sound start only after an input *)
      resume_audio ();
      (* the root <svg>, needed to convert mouse coordinates (see
       * adjust_x_y); None if the first frame is not drawn yet *)
      let svg_opt = Option.map snd !current in
      let evt_opt = js_event_to_event evt svg_opt in
      (match evt_opt with
      | None -> ()
      | Some event -> process_playground_event event
      );
      (* claude: the relative move too (mdx/mdy), from movementX/Y (not
       * in vdom's binding, hence Ojs), which keep counting when the
       * pointer is locked (captured, see playground3d's webgl backend),
       * whereas clientX/Y then stop; y up, like the playground's *)
      if Event.type_ evt = "mousemove" then begin
        let get prop = Ojs.float_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) prop) in
        process_playground_event (E.EMouseMoveBy (get "movementX", -. (get "movementY")))
      end;
      (* claude: a key press that produced a character also feeds
       * computer.keyboard.typed, beside the key event above *)
      if Event.type_ evt = "keydown" then begin
        match typed_of_key (Event.key evt) with
        | Some str -> process_playground_event (E.ETyped str)
        | None -> ()
      end;
      if !debug then Window.request_animation_frame window animation_frame;
    in
    [
      Event.Mousemove;
      Event.Mousedown;
      Event.Mouseup;
      Event.Keydown;
      Event.Keyup;
      Event.Wheel;
      Event.Dblclick;
    ] |> List.iter (fun evt_kind ->
       Window.add_event_listener window evt_kind on_js_event true
    );
    (* claude: a right click is the game's (Playground.mouse.mrdown), not
     * the browser's context menu *)
    Window.add_event_listener window Event.Contextmenu Event.prevent_default true;
  )
