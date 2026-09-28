(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_media.mli *)
open Playground

(*****************************************************************************)
(* Players *)
(*****************************************************************************)

type player = {
  url : string;
  mutable opened : (Media.media, string) result option;
  mutable playing : bool;
  mutable offset : float; (* where it is, while paused (seconds) *)
  mutable started : float; (* the browser's time when it was played *)
  mutable autoplayed : bool;
}

(* by the file's URL: the same file in two tabs is one player *)
let players : (string, player) Hashtbl.t = Hashtbl.create 8

let player (url : string) : player =
  match Hashtbl.find_opt players url with
  | Some p -> p
  | None ->
      let p = { url; opened = None; playing = false; offset = 0.; started = 0.; autoplayed = false } in
      Hashtbl.replace players url p;
      p

(* its media, opened when its bytes are first had *)
let opened (p : player) ~(media : string -> string option) : (Media.media, string) result =
  match p.opened with
  | Some r -> r
  | None -> (
      match media p.url with
      | Some "" -> Error "the file could not be had"
      | Some bytes ->
          let r = Result.map snd (Media.open_ ~name:p.url bytes) in
          p.opened <- Some r;
          r
      | None -> Error "loading...")

let sound_of (m : Media.media) : Signal.stereo option =
  match m with Sound s -> Some s.samples | Movie { sound = Some s; _ } -> Some s | _ -> None

(*****************************************************************************)
(* The deck: the sound playing *)
(*****************************************************************************)

type deck = { mutable samples : Signal.stereo option; mutable pos : int; mutable owner : string; mutable paused : bool }

let deck = { samples = None; pos = 0; owner = ""; paused = true }

let fill (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  Array.fill out.left 0 n 0.;
  Array.fill out.right 0 n 0.;
  match deck.samples with
  | Some s when not deck.paused ->
      let k = max 0 (min n (Array.length s.left - deck.pos)) in
      Array.blit s.left deck.pos out.left 0 k;
      Array.blit s.right deck.pos out.right 0 k;
      deck.pos <- deck.pos + k
  | _ -> ()

let install () : unit =
  ignore (Audio.instrument "tinychrome" (fun () -> { Instrument.note_on = (fun _ _ -> ()); note_off = ignore; set = (fun _ _ -> ()); fill }))

(*****************************************************************************)
(* Playing *)
(*****************************************************************************)

let duration (m : Media.media) : float = Option.value (Media.duration m) ~default:0.

(* where a player is: its sound's samples played, else the frame clock
 * since it was played; at its end, looped or stopped *)
let position (p : player) ~(now : float) ~(loop : bool) (m : Media.media) : float =
  let d = duration m in
  let t =
    if not p.playing then p.offset
    else if deck.owner = p.url && sound_of m <> None then float_of_int deck.pos /. float_of_int Signal.rate
    else p.offset +. (now -. p.started)
  in
  if d > 0. && t >= d then
    if loop then (
      p.offset <- 0.;
      p.started <- now;
      if deck.owner = p.url then deck.pos <- 0;
      Float.rem t d)
    else (
      p.playing <- false;
      p.offset <- 0.;
      if deck.owner = p.url then deck.paused <- true;
      0.)
  else t

let play (p : player) ~(now : float) (m : Media.media) : unit =
  p.playing <- true;
  p.started <- now;
  match sound_of m with
  | Some s ->
      deck.samples <- Some s;
      deck.pos <- int_of_float (p.offset *. float_of_int Signal.rate);
      deck.owner <- p.url;
      deck.paused <- false
  | None -> ()

let pause (p : player) ~(now : float) ~(loop : bool) (m : Media.media) : unit =
  p.offset <- position p ~now ~loop m;
  p.playing <- false;
  if deck.owner = p.url then deck.paused <- true

(*****************************************************************************)
(* The page's players *)
(*****************************************************************************)

(* a player's element's file, resolved: src=, else its first <source> *)
let source (page : Browser_page.t) (e : Dom.element) : string option =
  let src = match Dom.attribute "src" e with Some s -> Some s | None -> List.find_map (fun (c : Dom.element) -> Dom.attribute "src" c) (Dom.find_all "source" e) in
  Option.map (Browser_url.resolve page.url) src

let players_of (page : Browser_page.t) : (Html_layout.fragment * Dom.element) list =
  List.filter_map
    (fun (f : Html_layout.fragment) ->
      match f.picture with Some { src = ""; _ } when f.element.name = "video" || f.element.name = "audio" -> Some (f, f.element) | _ -> None)
    (Html_layout.fragments page.layout)

let mm_ss (t : float) : string = Printf.sprintf "%d:%02d" (int_of_float t / 60) (int_of_float t mod 60)

(* the time, in a small white (or dark) look *)
let label (text : string) ~(x : float) ~(baseline : float) (color : int * int * int) : shape list =
  let look = { Browser_text.root_look with size = 11.; color } in
  Browser_draw.glyphs { text; look; x; width = Browser_text.metrics look text; baseline; picture = None; control = None; element = Dom.element "span" [] }

(* a rectangle of the page (y down) *)
let fill_rect (c : color) (x : float) (y : float) (w : float) (h : float) : shape = rectangle c w h |> move (x +. (w /. 2.)) (-.(y +. (h /. 2.)))

let draw ~(now : float) ~(media : string -> string option) (page : Browser_page.t) : Browser_draw.drawn =
  List.concat_map
    (fun ((f : Html_layout.fragment), (e : Dom.element)) ->
      match (source page e, f.picture) with
      | Some url, Some pic ->
          let p = player url in
          let loop = Dom.attribute "loop" e <> None in
          let w = f.width and h = pic.height in
          let top = if pic.middle then f.baseline -. (h /. 2.) else f.baseline -. h in
          let video = e.name = "video" in
          let result = opened p ~media in
          (match result with
          | Ok m when (not p.autoplayed) && Dom.attribute "autoplay" e <> None ->
              p.autoplayed <- true;
              play p ~now m
          | _ -> ());
          let at = match result with Ok m -> position p ~now ~loop m | Error _ -> 0. in
          (* the picture, fitted and centred; or why there is none *)
          let picture =
            match result with
            | Ok (Movie { movie; _ }) when video ->
                let img = Movie.frame_at movie at in
                let k = Float.min (w /. float_of_int img.width) (h /. float_of_int img.height) in
                let iw = float_of_int img.width *. k and ih = float_of_int img.height *. k in
                [ bitmap iw ih img |> move (f.x +. (w /. 2.)) (-.(top +. (h /. 2.))) ]
            | Ok (Picture img) when video ->
                let k = Float.min (w /. float_of_int img.width) (h /. float_of_int img.height) in
                [ bitmap (float_of_int img.width *. k) (float_of_int img.height *. k) img |> move (f.x +. (w /. 2.)) (-.(top +. (h /. 2.))) ]
            | Error why when video -> label why ~x:(f.x +. 8.) ~baseline:(top +. (h /. 2.)) (200, 200, 200)
            | _ -> []
          in
          (* the controls: play or pause, how far, the time *)
          let controls =
            if (not video) || Dom.attribute "controls" e <> None then
              let d = match result with Ok m -> duration m | Error _ -> 0. in
              let bar_h = if video then 28. else h in
              let y = top +. h -. bar_h in
              let ink = if video then (255, 255, 255) else (32, 33, 36) in
              let ink_c = let r, g, b = ink in rgb r g b in
              let cy = y +. (bar_h /. 2.) in
              let icon =
                if p.playing then [ fill_rect ink_c (f.x +. 10.) (cy -. 6.) 4. 12.; fill_rect ink_c (f.x +. 17.) (cy -. 6.) 4. 12. ]
                else [ polygon ink_c [ (f.x +. 10., -.(cy -. 7.)); (f.x +. 10., -.(cy +. 7.)); (f.x +. 22., -.cy) ] ]
              in
              let track_x = f.x +. 32. and track_w = Float.max 10. (w -. 130.) in
              let done_ = if d > 0. then Float.min 1. (at /. d) else 0. in
              (if video then [ fill_rect (rgb 0 0 0) f.x y w bar_h |> fade 0.6 ] else [ fill_rect (rgb 241 243 244) f.x y w bar_h ])
              @ icon
              @ [ fill_rect (rgb 150 150 150) track_x (cy -. 2.) track_w 4.; fill_rect (rgb 255 0 0) track_x (cy -. 2.) (track_w *. done_) 4. ]
              @ label (mm_ss at ^ " / " ^ mm_ss d) ~x:(track_x +. track_w +. 10.) ~baseline:(cy +. 4.) ink
            else []
          in
          [ (top, top +. h, group (picture @ controls)) ]
      | _ -> [])
    (players_of page)

let click ~(now : float) ~(media : string -> string option) (page : Browser_page.t) (e : Dom.element) : bool =
  match source page e with
  | Some url when e.name = "audio" || Dom.attribute "controls" e <> None -> (
      let p = player url in
      let loop = Dom.attribute "loop" e <> None in
      match opened p ~media with
      | Ok m ->
          if p.playing then pause p ~now ~loop m else play p ~now m;
          true
      | Error _ -> true)
  | _ -> false
