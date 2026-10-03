(*****************************************************************************)
(* {1 Running an app} *)
(*****************************************************************************)

(* claude: [rendering] (default: Playground.default_rendering) sets how
 * to draw, see Playground.rendering; [flags] (default: none) are given
 * to the app's init, and so end up in computer.flags, see
 * Playground.flags *)
val run_app:
  ?rendering:Playground.rendering -> ?flags:Playground.flags -> ?network:< Cap.network ; .. > ->
  ?window:Playground.window -> ('a, 'b) Playground.app -> unit
(* claude: [window] (default: Playground.default_window, a game's): how
 * the window behaves -- the program's screen and whether it is the
 * window itself, the keys the platform keeps, whether a frame that did
 * not change is drawn again; see Playground.window, which an
 * application changes:
 *   run_app ~window:{ Playground.default_window with follows_window = true } app
 * (They were arguments of their own until 0.3.3: ?screen,
 * ?screen_follows_window, ?platform_keys.) *)
(* claude: [network], the program's capability to reach the network
 * (plan_caps.md), for what the platform does on its behalf: download
 * an image given by URL (Download.grant). A program granting it says
 * so in its main (run_app takes [< Cap.network; .. >]: only the network
 * of the capabilities it is given):
 *   let main = Cap.main (fun caps ->
 *     Playground_platform.run_app ~network:caps app)
 * Without it, an image URL is refused. (The Http.get command and
 * Multiplayer's net=host carry their own, given where they are made.) *)

(* The parameters the program was started with (see Playground.flags):
 * natively, the command line's arguments without a dash, name=value or
 * name; on the web, the page's URL parameters, ?name=value&name. The
 * one impure step of flags, visible in a program's main:
 *   let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
 *)
val flags: unit -> Playground.flags

(*****************************************************************************)
(* {1 The local time} *)
(*****************************************************************************)

(* claude: [utc_offset time]: the minutes the local clocks are ahead of
 * UTC at [time] (computer.time), for Clock.local: Paris +60 in winter
 * and +120 in summer, New York -300 and -240, India +330. Which one
 * applies when is the platform's to know: natively the C library's
 * (from $TZ, e.g. TZ=Asia/Kolkata, or the system's zone), on the web
 * the browser's. Natively 0 under -fixed-time, so that a golden frame
 * is the same wherever it is rendered. *)
val utc_offset: Playground.time -> int

(*****************************************************************************)
(* {1 The window's pixels} *)
(*****************************************************************************)

(* claude: [pixel_ratio ()]: how many of the window's own pixels one unit
 * of the program's screen is, now -- 1. for a window the screen's size,
 * 2.16 for tinybox's 1778 by 1000 screen in a 3840-pixel-wide window.
 *
 * Why: a program draws in its screen's units, and the platform scales the
 * whole picture to fit the window (run_app). Shapes and words are drawn by
 * the platform at the window's resolution, so they stay sharp at any size;
 * but a [bitmap] is pixels the program made, at the size it chose, and the
 * platform can only enlarge it (smoothed: blurred) to fill the window. A
 * program making its own pixels (tinybox's code map, whose letters are the
 * VGA font's, painted into one image) wants to make as many as the window
 * really shows, not fewer: with this ratio it makes its image [ratio]
 * times bigger than the units it covers, the platform shrinks it back by
 * [ratio], and each of its pixels is one of the window's. Before it, on a
 * big monitor, the map's code was painted at the screen's resolution and
 * blown up: a line 6 units high, each glyph squeezed into 4 by 7 pixels
 * then enlarged, unreadable, however many pixels the monitor had.
 *
 * It changes when the window does (dragged, full screen), so ask each
 * frame. Natively the Cairo window's scale; the software platform draws at
 * the screen's size (1.); the web says 1. for now. *)
val pixel_ratio: unit -> float

(*****************************************************************************)
(* {1 The mouse's cursor} *)
(*****************************************************************************)

(* The cursor shown over the window from now on (Playground.cursor says
 * what each is for). An effect, not a part of the view: a program
 * calls it when what is under the mouse changes (a browser, in its
 * update, when the pointer comes over a link), or once at the start (a
 * game that hides it). Asking for the one already shown costs nothing.
 * Natively SDL's system cursors; on the web the page's CSS cursor. *)
val set_cursor: Playground.cursor -> unit

(*****************************************************************************)
(* {1 The clipboard} *)
(*****************************************************************************)

(* The text every program of the desktop shares: what a copy puts
 * there ([set_clipboard]) and a paste takes ([clipboard], "" when it
 * holds no text). Effects, as [set_cursor]: a program calls them from
 * its update, on Ctrl+C and Ctrl+V. Natively SDL's. On the web a page
 * may write the clipboard but is only given it back later, by a
 * promise, and after asking the user: [clipboard] there is what this
 * program last wrote. *)
val clipboard: unit -> string
val set_clipboard: string -> unit

(*****************************************************************************)
(* {1 Images, loaded ahead} *)
(*****************************************************************************)

(* Load (and cache) an image url ahead of time, e.g. for all the sprite
 * variants a game will need, so that [Playground.image]/[run_app] never
 * has to load one lazily mid-game. On the web backend this starts an
 * asynchronous download and keeps the image in the browser's memory
 * cache, so switching a sprite to it later does not flicker; on the
 * native backend this blocks until the image is
 * downloaded and decoded, which is fine to do once up front but would
 * freeze the render loop if done lazily on first use. *)
val preload_image: string -> unit

(*****************************************************************************)
(* {1 Documents} *)
(*****************************************************************************)

(* Documents, saved and opened again (docs/claude_notes/plans/plan_io.md).
 *
 * A *store* of named documents, each a string of bytes (what
 * appkits/document/Saved writes): natively, the files of one directory
 * -- $ELM_PLAYGROUND_STORE if set, else ~/.elm-playground/documents --
 * and on the web the browser's localStorage, which lives in that
 * browser, for that site, and survives a reload. A name is the
 * document's own ("budget.sheet"); a '/' in it is not a directory.
 *
 * Each takes the capability it uses, which only [Cap.main] hands out,
 * once, in the program's main: a program whose main does not call it
 * cannot touch a document, and one that does says so in its types --
 *
 *   let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> File_menu.caps)))
 *
 * The platform itself is the trusted computing base: it holds no
 * capability, only asks for one. All four are synchronous, so an
 * [update] can call them as it goes. *)

(* [store caps name bytes]: kept under [name], over what was there *)
val store : < Cap.open_out; .. > -> string -> string -> unit

(* [fetch caps name]: what was stored under [name], if anything *)
val fetch : < Cap.open_in; .. > -> string -> string option

(* the names stored, in order *)
val stored : < Cap.readdir; .. > -> string list

(* [export caps name bytes]: a real file, out of the store --
 * natively written in the current directory, on the web downloaded *)
val export : < Cap.open_out; .. > -> string -> string -> unit
