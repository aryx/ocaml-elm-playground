(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Web_vdom.mli *)

open Js_browser


(* alt: when using the VDOM, but tedious to use request_animation_frame
 * and a global keyboard handler => switch to basic Dom (and mini-vdom later)
 *
 * module V = Vdom
 *)

(* when using directly the DOM *)

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

module Html = struct
let style = style
end

module Svg = struct
type 'msg t = 'msg vdom

let svg attrs xs = 
  svg_elt "svg" ~a:attrs xs

let trusted_node s attrs xs = 
  svg_elt s ~a:attrs xs

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
let viewBox = attr "viewBox"

let width = attr "width"
let height = attr "height"

let r = attr "r"
let rx = attr "rx"
let ry = attr "ry"
let fill = attr "fill"
let points = attr "points"
let transform = attr "transform"
let opacity = attr "opacity"

let href = attr "href"

let textAnchor = attr "text-anchor"
let dominantBaseline = attr "dominant-baseline"
let fontSize = attr "font-size"
let fontFamily = attr "font-family"
(* claude: not an attribute in Elm's Svg module (Elm uses a text child
 * node instead), but simpler with our tiny virtual DOM *)
let textContent = prop "textContent"

end
end
