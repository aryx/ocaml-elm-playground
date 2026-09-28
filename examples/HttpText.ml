(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A text fetched over HTTP, Elm's way: [init] returns a command,
 * Http.get, the platform performs it while the frames go on (the
 * square keeps turning), and the answer comes back to [update] as a
 * message, GotText. After the Elm guide's "HTTP" chapter, which fetches
 * a book; this one fetches its own source, from the server of
 * 'make serve-build':
 *
 *   make serve-build        (in another terminal)
 *   dune exec examples/HttpText.exe
 *   http://localhost:8001/examples/web/HttpText.html
 *   dune exec examples/HttpText.exe -- url=http://example.com/
 *
 * The network is the program's to grant (plan_caps.md): main gets the
 * capabilities from Cap.main, and the functions below take only the
 * network of them ([< Cap.network; .. >]).
 *
 * "r" asks again: a command can come from [update] too. Natively, the
 * request is networking/unix/Http_request's, stepped every frame
 * (http:// only); in a browser, an XMLHttpRequest. Without the server:
 * the error, as the program sees it. *)
open Playground

let default_url = "http://localhost:8001/examples/HttpText.ml"

type state = Loading | Success of string | Failure of string
type model = { url : string; state : state; time : float }
type msg = GotText of (string, Http.error) result | Tick of float | Key of string

let get (caps : < Cap.network ; .. >) (url : string) : msg Cmd.t =
  Http.get caps ~url ~expect:(Http.expect_string (fun result -> GotText result))

let init (caps : < Cap.network ; .. >) (flags : flags) : model * msg Cmd.t =
  let url = Option.value (List.assoc_opt "url" flags) ~default:default_url in
  ({ url; state = Loading; time = 0. }, get caps url)

let update (caps : < Cap.network ; .. >) (msg : msg) (model : model) : model * msg Cmd.t =
  match msg with
  | GotText (Ok text) -> ({ model with state = Success text }, Cmd.none)
  | GotText (Error e) -> ({ model with state = Failure (Http.error_to_string e) }, Cmd.none)
  | Tick time -> ({ model with time }, Cmd.none)
  | Key "r" -> ({ model with state = Loading }, get caps model.url)
  | Key _ -> (model, Cmd.none)

(* the text's first lines, cut to fit, tabs as spaces *)
let lines (text : string) : string list =
  String.split_on_char '\n' text
  |> List.filteri (fun i _ -> i < 60)
  |> List.map (fun line ->
         let line = String.map (fun c -> if c = '\t' || c = '\r' then ' ' else c) line in
         if String.length line > 140 then String.sub line 0 137 ^ "..." else line)

(* words starting at [x] rather than centered there: a character of
 * the default size is about 6 wide (0.6 em, as Widget.text_width) *)
let words_from (x : number) (y : number) (color : color) (s : string) : shape =
  words color s |> move (x +. (3. *. float_of_int (String.length s))) y

(* a line of source code: each character in a cell of its own, 6
 * wide, as a terminal would -- the words' font is proportional, and
 * code's indentation needs columns *)
let monospace (x : number) (y : number) (color : color) (s : string) : shape =
  String.to_seq s |> List.of_seq
  |> List.mapi (fun i c -> (i, c))
  |> List.filter (fun (_, c) -> c <> ' ')
  |> List.map (fun (i, c) -> words color (String.make 1 c) |> move (x +. (6. *. float_of_int i) +. 3.) 0.)
  |> group |> move_up y

let view (model : model) : shape list =
  let status, color, body =
    match model.state with
    | Loading -> ("loading...", gray, [])
    | Success text -> (Printf.sprintf "%d bytes" (String.length text), green, lines text)
    | Failure why -> ("failed", red, [ why ])
  in
  (* the frames go on while the request is in flight *)
  (square color 16. |> rotate (model.time *. 90.) |> move (-460.) 460.)
  :: words_from (-430.) 465. black ("GET " ^ model.url)
  :: words_from (-430.) 445. color status
  :: List.mapi (fun i line -> monospace (-460.) (410. -. (14. *. float_of_int i)) darkGray line) body

let app (caps : < Cap.network ; .. >) =
  {
    Playground.init = init caps;
    update = update caps;
    view;
    subscriptions =
      (fun _ -> Sub.batch [ Sub.on_animation_frame (fun t -> Tick t); Sub.on_key_down (fun key -> Key key) ]);
  }

let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app caps))
