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
 * thumbnails and the sources fetched by URL. *)

(* the host's type, the model and the menu's parts: Menu_model *)

(* [run ?network host]: the menu; [network], to fetch the thumbnails
 * when they are URLs (Playground_platform.run_app's). Its flags (the
 * command line, the URL's query): chosen=<Name> starts it on a program,
 * code=<Name> in that program's code map -- tinybox.html?code=TinyVi,
 * a link to a program's code to read *)
val run : ?network:< Cap.network ; .. > -> Menu_model.host -> unit
