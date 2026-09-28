(* tinybox's menu: every program of the catalogue, on two shelves
 * (games, apps), a section at a time, each with its screenshot, the
 * original it is after, what it is and what the original brought.
 * Enter starts the one chosen.
 *
 * The same menu natively and in a browser; what differs is its host's
 * (plan_tinybox_web.md): natively (native/), every program linked in,
 * one started in a process of its own (tinybox <Name>), the thumbnails
 * and the sources embedded, the chosen one previewed live; on the web
 * (web/), no program linked in, one started by loading its own page, the
 * thumbnails fetched by URL, no code map yet. *)

(* what the menu needs from where it runs *)
type host = {
  runnable : string list; (* the programs it can start (the others greyed) *)
  thumbnail : Catalogue.program -> Playground.number -> Playground.shape option;
  play : Catalogue.program -> string; (* start it; "", or why it could not *)
  running : unit -> string option; (* the program the menu waits for, if any *)
  ended : unit -> string option;
      (* once, when the one waited for ended: "", or how it failed *)
  sources : (string * string) list Lazy.t option;
      (* the repository's sources (path, text), for the code map; None: no
       * code map here *)
  preview : preview option; (* the chosen one, live in the panel *)
}

(* claude: the chosen program playing in the detail panel, after a
 * second on it (natively: Tinybox_native's "Previews") *)
and preview = {
  step : now:Playground.number -> dwell:int -> Catalogue.program option -> unit;
      (* a frame of the menu: [dwell] frames on the chosen one so far *)
  shapes : Catalogue.program -> Playground.shape list option;
      (* its view, in its 1000 by 1000, once it plays *)
  note : Catalogue.program -> string option; (* what to say of it, if anything *)
}

(* [run ?network host]: the menu; [network], to fetch the thumbnails
 * when they are URLs (Playground_platform.run_app's) *)
val run : ?network:< Cap.network ; .. > -> host -> unit
