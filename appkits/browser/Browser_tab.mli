(* Browser_tab: one page being browsed, and how it goes elsewhere --
   what a browser's window holds in each of its tabs.

   A browser is two things: its chrome (the title, the toolbar, the
   Location field: each browser's own) and what it shows, a **tab**: the
   page (loading, or shown and scrolled), the history behind and ahead,
   the pictures fetched and still to fetch, the form's field in focus,
   and -- when the browser runs scripts -- the page's script world
   (Browser_script), a JavaScript realm per page. TinyNetscape and
   TinyFirefox have one tab each; Firefox's tabs (2004, after Opera's
   and Mozilla's own) are a list of these, an exercise.

   Going somewhere:

     visit url     the page now kept in the history, then [load]
     load url      an about: page read at once (the built-in site); an
                   http:// or https:// one asked for (Cmd.Http_get), the
                   tab Loading until [got] answers
     got           the page read (Browser_page.read), its scripts run if
                   the browser has them, laid out again from the tree
                   they left; its pictures queued
     back, forward the history's two stacks (Browser_history): a page
                   kept is shown again at once, as it was scrolled

   The pictures come **four at a time** (Netscape's connections; Mosaic
   fetched one after the other): [fetch_more] keeps [connections] of them
   in flight, [got_picture] takes one in, decodes it, lays the page out
   again, and asks for the next. Stop forgets the rest.

   A page laid out by the box model (TinyChrome's) has its **style
   sheets** fetched the same way, ahead of its pictures: its <link
   rel=stylesheet>s, then the @imports of those that have come
   (Browser_page.sheets_wanted); [got_picture] takes a sheet in too
   (it is in [sheet_urls]), and the page is laid out again with it -- a
   page is shown at once, plain, and dressed as its sheets arrive,
   where Chrome waits for them (a few hundred milliseconds of a blank
   window, to avoid that flash).

   What varies between browsers is a [config]: their looks (the page's
   settings: Netscape's extensions, CSS), their messages (what a
   response is turned into), their built-in site, the page area's size,
   whether they run scripts. *)

type state = Loading of string | Shown of Browser_page.t
type view = Page | Source

(* a page in the history: where, and itself if it is kept (with its
 * scripts' world), and how far down it was *)
type entry = { at : string; kept : (Browser_page.t * Browser_script.t option) option; scrolled_to : int }

type t = {
  state : state;
  view : view;
  scroll : int; (* the first line shown, of [config.line_height] *)
  history : entry Browser_history.t;
  visited : string list;
  fragment : string option; (* a #name to scroll to once shown *)
  pictures : (string * Browser_picture.t) list; (* by URL, every page's: a cache *)
  sheets : (string * string) list; (* the style sheets' texts by URL, "" for one that could not be had: a cache *)
  sheet_urls : string list; (* the URLs asked for as style sheets *)
  queue : string list; (* the page's pictures still to fetch *)
  in_flight : string list; (* on their way *)
  total : int; (* the page's pictures to fetch, for the progress *)
  images : bool; (* Auto Load Images *)
  focus : Dom.element option; (* a form's field typed into *)
  script : Browser_script.t option; (* the page's scripts, if the browser runs them *)
}

type 'msg config = {
  settings : t -> Browser_page.settings; (* the page's looks: the browser's, the tab's visited links and pictures *)
  about : string -> (string * string) option; (* the built-in site: about:NAME's bytes and type *)
  got : string -> (Playground.Http.response, Playground.Http.error) result -> 'msg;
  got_picture : string -> (Playground.Http.response, Playground.Http.error) result -> 'msg;
  connections : int; (* pictures at a time *)
  visible : int; (* the page area's lines *)
  line_height : float;
  scripts : string -> bool; (* whether a page's <script>s run (Browser_script), by its URL *)
  seed : int; (* Math.random's *)
}

(* nothing shown yet; pictures loaded or not *)
val empty : images:bool -> t
val current_url : t -> string

(* the scroll moved by [by] lines, kept within the page *)
val scrolled : 'msg config -> int -> t -> t

(* laid out again: the same tree, the browser's looks changed (CSS off) *)
val relaid : 'msg config -> t -> t

val load : ?post:string * string -> 'msg config -> < Cap.network ; .. > -> string -> t -> t * 'msg Cmd.t
val visit : ?post:string * string -> 'msg config -> < Cap.network ; .. > -> string -> t -> t * 'msg Cmd.t
val back : 'msg config -> < Cap.network ; .. > -> t -> t * 'msg Cmd.t
val forward : 'msg config -> < Cap.network ; .. > -> t -> t * 'msg Cmd.t

(* not waiting any more: a page loading shown as stopped, the pictures
 * on their way forgotten *)
val stop : 'msg config -> t -> t

(* the pictures loaded now (Netscape's Images button) *)
val load_images : 'msg config -> < Cap.network ; .. > -> t -> t * 'msg Cmd.t

(* a page's answer: read and shown, or the page saying why not *)
val got : 'msg config -> < Cap.network ; .. > -> string -> (Playground.Http.response, Playground.Http.error) result -> t -> t * 'msg Cmd.t

(* a picture's answer: decoded (or broken), the page laid out again, the
 * next one asked for *)
val got_picture : 'msg config -> < Cap.network ; .. > -> string -> (Playground.Http.response, Playground.Http.error) result -> t -> t * 'msg Cmd.t

(* what a form's click or key did (Browser_forms): the focus moved, the
 * page's values changed (its script told, if it has one), or the form
 * sent *)
val form_effect : 'msg config -> < Cap.network ; .. > -> keep_focus:bool -> Browser_forms.effect -> t -> t * 'msg Cmd.t

(* after a script's task (a click, a key, a timer): if the tree changed,
 * the page laid out again from it, the field in focus found again in
 * it, new pictures asked for *)
val after_task : 'msg config -> < Cap.network ; .. > -> t -> t * 'msg Cmd.t
