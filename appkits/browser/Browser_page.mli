(* Browser_page: a page, read and laid out -- the whole pipeline of
 * libs/web/ in one place, from the bytes a server sent to the shapes
 * a browser shows:
 *
 *   bytes -Charset-> text -Html_lexer-> tokens -Html_tree-> tree
 *         -Looks, Html_layout-> boxes -Browser_draw-> shapes
 *
 * keeping every stage (a browser's views show each), and what the
 * person did to it: its form controls' values, the browser's, not the
 * tree's.
 *
 * What the layout and the drawing need that the page does not say is
 * the browser's, given as [settings]: the page's width, how lines are
 * broken, which URLs were visited (a link purple), which pictures have
 * come -- a page is laid out again when any of them changes (a reflow).
 *
 * And the pages a browser writes itself, laid out like any: what is not
 * HTML made one (text in <pre>, as Mosaic showed it), an error, a
 * form's echo (what a server would read). *)

type t = {
  url : string; (* where it came from, after the redirections *)
  status : int; (* 200; 0 for a page that could not be had *)
  charset : Charset.t;
  bytes : int;
  lines : string list; (* the text, UTF-8, tabs expanded: its source *)
  tokens : Html_lexer.token list;
  tree : Dom.element;
  line_mode : Line_mode.t;
  title : string; (* the text of its <title>, or "" *)
  layout : Html_layout.box;
  drawn : Browser_draw.drawn; (* all but its controls, drawn once a layout *)
  forms : Forms.form list;
  values : (Dom.element * Forms.value) list; (* the controls changed, by element (==) *)
}

type settings = {
  width : float;
  breaker : Html_layout.breaker;
  visited : string -> bool; (* an absolute URL, no #fragment *)
  picture : string -> Browser_picture.t option; (* an absolute URL *)
}

(* Knuth and Plass's lines (Linebreak.optimal), ragged right as a
 * browser's are -- the spaces may stretch (a line may end short), not
 * shrink (it may not end past the edge) -- with the paragraph's first
 * real space for all (Linebreak's model has one): CSS's text-wrap:
 * pretty *)
val pretty : Html_layout.breaker

(* [read settings url status content_type bytes]: the page, through the
 * whole pipeline *)
val read : settings -> string -> int -> string option -> string -> t

(* the same tree laid out and drawn again: a reflow *)
val laid_out : settings -> t -> t

(* a control's value now: as typed and clicked, else as the page gave
 * it *)
val value_of : t -> Dom.element -> Forms.value

val with_value : t -> Dom.element -> Forms.value -> t

(* a response's media type: "text/html; charset=utf-8" is "text/html" *)
val media_type : string option -> string

(* a page that could not be had, laid out like any: its URL, why *)
val error_html : string -> string -> string

(* a form's fields, as a server would read them: the method ("GET",
 * "POST") and what was sent, encoded (the query or the body), decoded
 * too *)
val echo_html : string -> string -> string
