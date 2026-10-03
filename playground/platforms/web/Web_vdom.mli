(* Web_vdom: a tiny virtual DOM, the web platform's own (about 100
 * lines of diff and patch), for one job: a frame's shapes as SVG.
 *
 * The DOM is the tree of live elements the browser displays (<svg>,
 * <circle>, <image>, ...). A virtual DOM is a plain value describing
 * such a tree, [t] below: it costs nothing to build a new one each
 * frame, and it is not displayed. [patch] compares this frame's
 * description with the previous one and changes only what differs in
 * the real elements.
 *
 * Why not rebuild the <svg> each frame, which is simpler: a new <image>
 * element decodes its picture asynchronously, even from the browser's
 * cache, so a sprite disappeared for a frame now and then, and an
 * animated GIF restarted at its first frame, never animating. Patched,
 * Mario's <image> is the same element frame after frame, its href
 * changed only when the sprite does.
 *
 * Why not ocaml-vdom's Vdom, which diffs too: it owns the main loop,
 * and gives neither a tick per animation frame nor the keys of the whole
 * window (Playground_platform.ml's prelude tells that story). Only its
 * Js_browser, the bindings to the browser, is used.
 *
 * Simpler than Elm's or Vdom's: children are matched by position, the
 * first of the old frame with the first of the new, no keys. That fits
 * the Playground, whose views give the same shapes in the same order
 * frame after frame, moved and recoloured.
 *)

(* an element's attribute in the markup (r="10"), a property of its
 * style (position: fixed), or a JavaScript property of the element
 * object, used for "textContent", the text inside a <text> *)
type attr = Attr of string * string | Style of string * string | Prop of string * string

(* an element described: its tag, attributes and children *)
type t = { tag : string; attrs : attr list; children : t list }

(* Elm's Svg msg, the message type a phantom: no node here sends one *)
type 'a vdom = t

val attr : string -> string -> attr
val style : string -> string -> attr
val prop : string -> string -> attr

(* an element of the SVG namespace *)
val svg_elt : string -> a:attr list -> t list -> t

(* real elements built from a description: the first frame's, and any
 * part of the tree the previous frame did not have *)
val create : t -> Js_browser.Element.t

(* [patch ~parent elt old node]: [elt], built from [old], made to show
 * [node]: attributes no longer there removed, those new or changed
 * set, the children patched in turn (more of them created, fewer
 * removed). An element of another tag is replaced. Returns the element
 * now in the page, [elt] unless it was replaced *)
val patch : parent:Js_browser.Element.t -> Js_browser.Element.t -> t -> t -> Js_browser.Element.t

(* Elm's Html and Svg modules, the few names the renderer uses *)

module Html : sig
  val style : string -> string -> attr
end

module Svg : sig
  type 'msg t = 'msg vdom

  val svg : attr list -> 'msg t list -> 'msg t
  val circle : attr list -> 'msg t list -> 'msg t
  val ellipse : attr list -> 'msg t list -> 'msg t
  val rect : attr list -> 'msg t list -> 'msg t
  val polygon : attr list -> 'msg t list -> 'msg t
  val image : attr list -> 'msg t list -> 'msg t
  val text_ : attr list -> 'msg t list -> 'msg t

  (* a group: its attributes (transform, opacity) apply to all its
   * children *)
  val g : attr list -> 'msg t list -> 'msg t

  (* any other SVG element, by its tag *)
  val trusted_node : string -> attr list -> 'msg t list -> 'msg t

  module Attributes : sig
    val viewBox : string -> attr
    val width : string -> attr
    val height : string -> attr
    val r : string -> attr
    val rx : string -> attr
    val ry : string -> attr
    val fill : string -> attr
    val points : string -> attr
    val transform : string -> attr
    val opacity : string -> attr
    val href : string -> attr
    val textAnchor : string -> attr
    val dominantBaseline : string -> attr
    val fontSize : string -> attr
    val fontFamily : string -> attr

    (* not an attribute in Elm's Svg (a text child node there): the
     * element's textContent property, simpler here *)
    val textContent : string -> attr
  end
end
