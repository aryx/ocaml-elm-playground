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

let kind = "text"
let pad = 6.

type state = { r : Rich.t; was : string list; was_down : bool }

let laid_out r width = Page.layout ~metrics:Stroke_text.metrics ~width:(width -. (2. *. pad)) r

(* the page's coordinates (y down from the top of the text) and the
   playground's, for a text in box b *)
let on_screen (b : Widget.box) (x, y) = (Widget.left b +. pad +. x, Widget.top b -. pad -. y)
let off_screen (b : Widget.box) (x, y) = (x -. Widget.left b -. pad, Widget.top b -. pad -. y)

(* the looks written down: how many runs, a line per run -- its length
   and its looks -- and then the characters *)
let save r =
  let runs = Rich.runs r in
  String.concat ""
    (Printf.sprintf "%d\n" (List.length runs)
     :: List.map
          (fun (_, len, (st : Style.t)) ->
            let b x = if x then "1" else "0" in
            Printf.sprintf "%d %s%s%s%s %g\n" len (b st.bold) (b st.italic) (b st.underline) (b st.strike) st.size)
          runs)
  ^ Rich.to_string r

let command c st =
  let r =
    match c with
    | "Bold" -> Rich.restyle Style.toggle_bold st.r
    | "Italic" -> Rich.restyle Style.toggle_italic st.r
    | "Underline" -> Rich.restyle Style.toggle_underline st.r
    | "Bigger" -> Rich.restyle (fun s -> { s with size = s.size *. 1.25 }) st.r
    | "Smaller" -> Rich.restyle (fun s -> { s with size = s.size /. 1.25 }) st.r
    | "Plain" -> Rich.restyle (fun s -> { Style.plain with size = s.size }) st.r
    | _ -> st.r
  in
  { st with r }

let input computer (b : Widget.box) st =
  let m = computer.mouse and k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key st.was) in
  let page = laid_out st.r b.w in
  let at () = Page.offset_at page (off_screen b (m.mx, m.my)) in
  let r = st.r in
  let r =
    if m.mdown && not st.was_down then if Widget.contains b m.mx m.my then Rich.at (at ()) r else r
    else if m.mdown && st.was_down then Rich.to_ (at ()) r
    else r
  in
  let text = Rich.to_string r in
  let r =
    if List.mem "Control" now then
      if pressed "b" then (command "Bold" { st with r }).r
      else if pressed "i" then (command "Italic" { st with r }).r
      else if pressed "u" then (command "Underline" { st with r }).r
      else r
    else if k.typed <> "" then Rich.insert k.typed r
    else if pressed "Enter" then Rich.insert "\n" r
    else if pressed "Backspace" then Rich.delete_backward r
    else if pressed "ArrowLeft" then Rich.at (Text.prev_char text (Rich.caret r)) r
    else if pressed "ArrowRight" then Rich.at (Text.next_char text (Rich.caret r)) r
    else r
  in
  { r; was = now; was_down = m.mdown }

let draw st (b : Widget.box) ~active =
  let page = laid_out st.r b.w in
  let ink = rgb 20 20 20 in
  let a, z = Rich.range st.r in
  let glyphs =
    List.concat_map
      (fun (g : Page.glyph) ->
        let x, y = on_screen b (g.x, g.baseline) in
        let shade =
          if active && g.offset >= a && g.offset < z && g.text <> "\n" then
            [ rectangle (rgb 170 200 240) g.advance (g.style.size *. 1.2) |> move (x +. (g.advance /. 2.)) (y +. (g.style.size *. 0.3)) ]
          else []
        in
        shade @ if g.text = "\n" || g.text = " " then [] else Stroke_text.glyph ink g.style g.text ~x ~baseline:y)
      (Page.glyphs page)
  in
  let caret =
    if active && a = z then
      let x, baseline, height = Page.caret_at page (Rich.caret st.r) in
      let x, y = on_screen b (x, baseline) in
      [ rectangle ink 2. (height *. 0.8) |> move x (y +. (height *. 0.25)) ]
    else []
  in
  glyphs @ caret

let rec part st : Component.part =
  {
    kind;
    height = (fun w -> Page.height (laid_out st.r w) +. (2. *. pad));
    draw = draw st;
    input = (fun computer b -> part (input computer b st));
    menu = [ "Text"; "Bold"; "Italic"; "Underline"; "Bigger"; "Smaller"; "Plain" ];
    command = (fun c -> part (command c st));
    save = (fun () -> save st.r);
  }

let make r = part { r; was = []; was_down = false }

let load s =
  let lines = String.split_on_char '\n' s in
  let n = int_of_string (List.hd lines) in
  let runs = List.filteri (fun i _ -> i >= 1 && i <= n) lines in
  (* the characters are everything after the run lines, newlines and all *)
  let skip = List.fold_left (fun acc l -> acc + String.length l + 1) 0 (List.filteri (fun i _ -> i <= n) lines) in
  let text = String.sub s skip (String.length s - skip) in
  let r, _ =
    List.fold_left
      (fun (r, start) line ->
        Scanf.sscanf line "%d %c%c%c%c %g" (fun len b i u k size ->
            let st : Style.t = { bold = b = '1'; italic = i = '1'; underline = u = '1'; strike = k = '1'; size } in
            (Rich.restyle (fun _ -> st) (Rich.select ~anchor:start ~caret:(start + len) r), start + len)))
      (Rich.of_string text, 0) runs
  in
  make (Rich.at 0 r)
