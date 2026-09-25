(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Textmode.mli *)

type 'model state = {
  model : 'model;
  (* the screen the VT100 shows, and the VT100 *)
  shown : Curses.t;
  vt : Vt.t;
  keys : unit Scene2d.t;
  (* the bytes sent at the last frame, and what a redraw would have cost *)
  sent : int;
  whole : int;
}

let start (p : 'model Tui.program) (keys : unit Scene2d.t) : 'model state =
  let screen = p.view p.init in
  let bytes = Curses.redraw screen in
  { model = p.init; shown = screen; vt = Vt.feed (Vt.create ~rows:(Curses.rows screen) ~cols:(Curses.cols screen)) bytes; keys;
    sent = String.length bytes; whole = String.length bytes }

let textmode ?phosphor (p : 'model Tui.program) : ('model state game, msg) app =
  let update (computer : computer) (s : 'model state) : 'model state =
    let before = s.keys.keys in
    let keys = Scene2d.update computer s.keys in
    if p.over s.model then if Scene2d.pressed (fun k -> k.kenter) keys then start p keys else { s with keys; sent = 0 }
    else begin
      let (Time now) = computer.time in
      (* a frame more than a quarter of a second late is a pause (the
         window hidden, the clock jumping), not time played *)
      let dt = match s.keys.last with None -> 0. | Some last -> Float.min 0.25 (now -. last) in
      let typed = Line_discipline.split_keys (Teletype.keyboard_bytes computer ~before) in
      let model = List.fold_left (fun m k -> p.update (Key k) m) s.model typed in
      let model = p.update (Tick dt) model in
      let screen = p.view model in
      let bytes = Curses.refresh ~before:s.shown screen in
      { model; shown = screen; vt = Vt.feed s.vt bytes; keys; sent = String.length bytes; whole = String.length (Curses.redraw screen) }
    end
  in
  let view (computer : computer) (s : 'model state) : shape list =
    let _, h = Teletype.screen_size computer s.vt in
    let note =
      if p.over s.model then "(over -- Enter to start again)"
      else Printf.sprintf "curses sent %d bytes this frame; a whole redraw, %d" s.sent s.whole
    in
    (rectangle (rgb 10 14 10) computer.screen.width computer.screen.height :: Teletype.draw_screen ?phosphor ~cursor:true computer s.vt)
    @ [ words (rgb 90 120 90) note |> move_y (-.(h /. 2.) -. 20.) ]
  in
  game view update (start p (Scene2d.start ()))
