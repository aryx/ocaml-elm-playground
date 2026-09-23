(*****************************************************************************)
(* {1 Running an app} *)
(*****************************************************************************)

(* claude: [rendering] (default: Playground.default_rendering) sets how
 * to draw, see Playground.rendering; [flags] (default: none) are given
 * to the app's init, and so end up in computer.flags, see
 * Playground.flags *)
val run_app:
  ?rendering:Playground.rendering -> ?flags:Playground.flags -> ?network:< Cap.network ; .. > ->
  ('a, 'b) Playground.app -> unit
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
