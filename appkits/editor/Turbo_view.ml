(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Turbo_view.mli *)

open Turbo_model

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let attrs ?(bold = false) fg bg : Vt.attrs = { Vt.plain with fg; bg; bold }
let grey = attrs Vt.Black Vt.White
let grey_hot = attrs Vt.Red Vt.White
let chosen = attrs Vt.Black Vt.Green
let chosen_hot = attrs Vt.Red Vt.Green
let frame_attrs = attrs ~bold:true Vt.White Vt.Blue
let text_attrs = attrs ~bold:true Vt.Yellow Vt.Blue
let reserved_attrs = attrs ~bold:true Vt.White Vt.Blue
let comment_attrs = attrs Vt.White Vt.Blue
let literal_attrs = attrs ~bold:true Vt.Cyan Vt.Blue
let error_attrs = attrs ~bold:true Vt.White Vt.Red
let dialog_frame = attrs ~bold:true Vt.White Vt.White

let fill (top : int) (left : int) (h : int) (w : int) (a : Vt.attrs) (s : Curses.t) : Curses.t =
  let row = String.make w ' ' in
  let s = ref s in
  for r = top to top + h - 1 do
    s := Curses.put ~attrs:a r left row !s
  done;
  !s

(* a frame of the PC's box characters, its title centred in its top *)
let frame ?(double = true) ?(title = "") (top : int) (left : int) (h : int) (w : int) (a : Vt.attrs) (s : Curses.t) : Curses.t =
  let hz, vt, tl, tr, bl, br = if double then ("═", "║", "╔", "╗", "╚", "╝") else ("─", "│", "┌", "┐", "└", "┘") in
  let bar = String.concat "" (List.init (w - 2) (fun _ -> hz)) in
  let s = Curses.put ~attrs:a top left (tl ^ bar ^ tr) s in
  let s = Curses.put ~attrs:a (top + h - 1) left (bl ^ bar ^ br) s in
  let s = ref s in
  for r = top + 1 to top + h - 2 do
    s := Curses.put ~attrs:a r left vt !s;
    s := Curses.put ~attrs:a r (left + w - 1) vt !s
  done;
  if title = "" then !s else Curses.put ~attrs:a top (left + ((w - String.length title - 2) / 2)) (" " ^ title ^ " ") !s

(* the shadow a window throws, two columns right and a row down: what
   is under it, dark grey on black *)
let shadow (top : int) (left : int) (h : int) (w : int) (s : Curses.t) : Curses.t =
  let dark = attrs ~bold:true Vt.Black Vt.Black in
  let shade s r c = if r < Curses.rows s && c < Curses.cols s then Curses.put ~attrs:dark r c (Curses.cell s r c).glyph s else s in
  let s = ref s in
  for r = top + 1 to top + h do
    s := shade (shade !s r (left + w)) r (left + w + 1)
  done;
  for c = left + 2 to left + w - 1 do
    s := shade !s (top + h) c
  done;
  !s

(* a label with its hot letter in another colour *)
let hot_label (r : int) (c : int) (label : string) (hot : char) (a : Vt.attrs) (ha : Vt.attrs) (s : Curses.t) : Curses.t =
  let s = Curses.put ~attrs:a r c label s in
  match String.index_opt label hot with Some i -> Curses.put ~attrs:ha r (c + i) (String.make 1 hot) s | None -> s

(* the columns of the menu bar's titles *)
let bar_columns : int list =
  let rec go c = function [] -> [] | (t, _) :: rest -> c :: go (c + String.length t + 2) rest in
  go 2 Turbo_menus.menus

let menu_bar (open_ : int option) (s : Curses.t) : Curses.t =
  let s = fill 0 0 1 80 grey s in
  List.fold_left2
    (fun s (i, (title, _)) c ->
      let a, ha = if open_ = Some i then (chosen, chosen_hot) else (grey, grey_hot) in
      hot_label 0 (c - 1) (" " ^ title ^ " ") title.[0] a ha s)
    s
    (List.mapi (fun i menu -> (i, menu)) Turbo_menus.menus)
    bar_columns

let status_line (m : model) (s : Curses.t) : Curses.t =
  let s = fill 23 0 1 80 grey s in
  let keys =
    match (m.session, m.mode) with
    | Some _, Executing -> [ ("", "Running..."); ("Ctrl+C", "Break") ]
    | Some _, _ -> [ ("F7", "Trace"); ("F8", "Step"); ("F4", "Here"); ("Ctrl+F9", "Run"); ("Ctrl+F2", "Reset"); ("Ctrl+F7", "Watch") ]
    | None, _ -> [ ("F1", "Help"); ("F2", "Save"); ("F3", "Open"); ("Alt+F9", "Compile"); ("F9", "Make"); ("Ctrl+F9", "Run"); ("F10", "Menu") ]
  in
  fst
    (List.fold_left
       (fun (s, c) (k, what) ->
         let s = Curses.put ~attrs:grey_hot 23 c k s in
         let s = Curses.put ~attrs:grey 23 (c + String.length k + 1) what s in
         (s, c + String.length k + String.length what + 3))
       (s, 1) keys)

(* a line cut into its colours: reserved words, comments, strings and
   numbers. [comment] is the closing of a comment running on from the
   line before ('}', or ')' for a star-parenthesis), and the same is
   returned for the next line *)
let colour_line (l : string) (comment : char option) : (int * string * Vt.attrs) list * char option =
  let n = String.length l in
  let out = ref [] in
  let add start stop a = if stop > start then out := (start, String.sub l start (stop - start), a) :: !out in
  (* where a comment closed by [close] ends, searched from [from] *)
  let rec ending close from =
    if from >= n then None
    else if close = '}' && l.[from] = '}' then Some (from + 1)
    else if close = ')' && l.[from] = '*' && from + 1 < n && l.[from + 1] = ')' then Some (from + 2)
    else ending close (from + 1)
  in
  let rec go i comment =
    if i >= n then comment
    else
      match comment with
      | Some close -> in_comment i close i
      | None ->
          let c = l.[i] in
          if c = '{' then in_comment i '}' (i + 1)
          else if c = '(' && i + 1 < n && l.[i + 1] = '*' then in_comment i ')' (i + 2)
          else if c = '\'' then begin
            let rec close j = if j >= n then n else if l.[j] = '\'' then j + 1 else close (j + 1) in
            let stop = close (i + 1) in
            add i stop literal_attrs;
            go stop None
          end
          else if Turbo_edit.is_word c then begin
            let rec stop j = if j < n && Turbo_edit.is_word l.[j] then stop (j + 1) else j in
            let j = stop i in
            let w = String.lowercase_ascii (String.sub l i (j - i)) in
            add i j (if List.mem w Pascal_lexer.keywords then reserved_attrs else if c >= '0' && c <= '9' then literal_attrs else text_attrs);
            go j None
          end
          else begin
            add i (i + 1) text_attrs;
            go (i + 1) None
          end
  and in_comment start close from =
    match ending close from with
    | Some stop ->
        add start stop comment_attrs;
        go stop None
    | None ->
        add start n comment_attrs;
        Some close
  in
  let next = go 0 comment in
  (List.rev !out, next)

let exec_attrs = attrs Vt.Black Vt.Cyan
let break_attrs = attrs ~bold:true Vt.White Vt.Red

let edit_window (m : model) (s : Curses.t) : Curses.t =
  let h = 22 - Turbo_edit.watch_rows m in
  let s = fill 1 0 h 80 text_attrs s in
  let s = frame ~title:m.file 1 0 h 80 frame_attrs s in
  let pos = Printf.sprintf " %s%d:%d " (if m.modified then "* " else "") (m.row + 1) (m.col + 1) in
  let s = Curses.put ~attrs:frame_attrs h 3 pos s in
  (* the comments running into the window from above it *)
  let comment = ref None in
  for r = 0 to m.top - 1 do
    comment := snd (colour_line (Turbo_edit.line m r) !comment)
  done;
  let s = ref s in
  let bar = Turbo_debug.execution_line m in
  for i = 0 to Turbo_edit.text_rows m - 1 do
    let r = m.top + i in
    if r < Turbo_edit.nlines m then begin
      let pieces, next = colour_line (Turbo_edit.line m r) !comment in
      comment := next;
      (* the execution bar, or a breakpoint: the whole line in its colour *)
      let whole = if bar = Some r then Some exec_attrs else if List.mem (r + 1) m.breakpoints then Some break_attrs else None in
      let pieces = match whole with Some a -> List.map (fun (st, piece, _) -> (st, piece, a)) pieces | None -> pieces in
      (match whole with Some a -> s := Curses.put ~attrs:a (2 + i) 1 (String.make Turbo_edit.text_cols ' ') !s | None -> ());
      List.iter
        (fun (start, piece, a) ->
          String.iteri
            (fun k ch ->
              let c = start + k - m.left in
              if c >= 0 && c < Turbo_edit.text_cols then s := Curses.put ~attrs:a (2 + i) (1 + c) (String.make 1 ch) !s)
            piece)
        pieces
    end
  done;
  (* the error, in a red bar over the window's first line *)
  match m.error with Some e -> Curses.put ~attrs:error_attrs 2 1 (Printf.sprintf " %-77s" e) !s | None -> !s

(* the watches, each with its value in the paused program *)
let watch_window (m : model) (s : Curses.t) : Curses.t =
  let h = Turbo_edit.watch_rows m in
  if h = 0 then s
  else
    let top = 23 - h in
    let window = attrs Vt.Black Vt.Cyan in
    let s = fill top 0 h 80 window s in
    let s = frame ~double:false ~title:"Watches" top 0 h 80 window s in
    let value w =
      match m.session with
      | Some sess when sess.goal = None -> Pdebug.watch sess.program sess.machine w
      | Some _ -> "(running)"
      | None -> "(no program running: F7 or F8 starts it)"
    in
    List.fold_left
      (fun s (i, w) -> if i < h - 2 then Curses.put ~attrs:window (top + 1 + i) 2 (let t = w ^ ": " ^ value w in if String.length t > 76 then String.sub t 0 76 else t) s else s)
      s
      (List.mapi (fun i w -> (i, w)) m.watches)

let dropdown (bar : int) (sel : int) (s : Curses.t) : Curses.t =
  let items = snd (List.nth Turbo_menus.menus bar) in
  let label_w = List.fold_left (fun w it -> max w (String.length it.label)) 0 items in
  let short_w = List.fold_left (fun w it -> max w (String.length it.shortcut)) 0 items in
  let w = label_w + short_w + 6 and h = List.length items + 2 in
  let left = List.nth bar_columns bar - 1 in
  let s = fill 1 left h w grey s in
  let s = frame ~double:false 1 left h w grey s in
  let s =
    List.fold_left
      (fun s (i, it) ->
        let a, ha = if i = sel then (chosen, chosen_hot) else (grey, grey_hot) in
        let text = Printf.sprintf " %-*s  %*s " label_w it.label short_w it.shortcut in
        hot_label (2 + i) (left + 1) text it.hot a ha s)
      s
      (List.mapi (fun i it -> (i, it)) items)
  in
  shadow 1 left h w s

let dialog (title : string) (h : int) (w : int) (s : Curses.t) : Curses.t * int * int =
  let top = (24 - h) / 2 and left = (80 - w) / 2 in
  let s = fill top left h w grey s in
  let s = frame ~title top left h w dialog_frame s in
  (shadow top left h w s, top, left)

let button (r : int) (c : int) (label : string) (s : Curses.t) : Curses.t = Curses.put ~attrs:chosen r c (" " ^ label ^ " ") s

let vt_screen (vt : Vt.t) : Curses.t =
  let s = ref (Curses.create ~rows:24 ~cols:80) in
  for r = 0 to min 23 (Vt.rows vt - 1) do
    for c = 0 to min 79 (Vt.cols vt - 1) do
      let cell = Vt.cell vt r c in
      if cell.glyph <> " " || cell.attrs <> Vt.plain then s := Curses.put ~attrs:cell.attrs r c cell.glyph !s
    done
  done;
  !s

let listing (m : model) (first : int) (s : Curses.t) : Curses.t =
  let p = match m.compiled with Some p -> p | None -> { Pcode.code = [||]; lines = [||]; statements = [||]; procedures = [||] } in
  let top = 3 and left = 8 and h = 18 and w = 64 in
  let window = attrs Vt.Black Vt.Cyan in
  let s = fill top left h w window s in
  let s = frame ~title:("P-code: " ^ m.file) top left h w (attrs ~bold:true Vt.White Vt.Cyan) s in
  let s = ref (shadow top left h w s) in
  for i = 0 to h - 3 do
    let a = first + i in
    if a < Array.length p.code then begin
      let here = p.lines.(a) = m.row + 1 in
      (* where a paused program is, marked *)
      let at_pc = match m.session with Some sess when sess.goal = None -> Pmachine.pc sess.machine = a | _ -> false in
      let text = Printf.sprintf "%s%4d  %-24s line %d" (if at_pc then ">" else " ") a (Pcode.show p.code.(a)) p.lines.(a) in
      s := Curses.put ~attrs:(if here then attrs ~bold:true Vt.Yellow Vt.Blue else window) (top + 1 + i) (left + 1) (Printf.sprintf "%-*s" (w - 2) text) !s
    end
  done;
  Curses.put ~attrs:(attrs ~bold:true Vt.White Vt.Cyan) (top + h - 1) (left + 2) " the cursor's line highlighted; Esc " !s

(* the call stack: each frame's call, and its links -- the static one
   to where its procedure was declared, the dynamic one to its caller *)
let stack_window (m : model) (s : Curses.t) : Curses.t =
  match m.session with
  | None -> s
  | Some sess ->
      let frames = Pdebug.frames sess.program sess.machine in
      let lines =
        List.map
          (fun (f : Pdebug.frame) ->
            if f.procedure = 0 then Printf.sprintf "%-16s frame %d" (Pdebug.call sess.program sess.machine f) f.base
            else Printf.sprintf "%-16s frame %-5d static link %-5d dynamic link %d" (Pdebug.call sess.program sess.machine f) f.base f.static_link f.dynamic_link)
          frames
      in
      let lines = List.filteri (fun i _ -> i < 12) lines @ [ ""; "static link: the frame of the procedure it is in"; "dynamic link: the frame of its caller" ] in
      let w = 66 in
      let s, top, left = dialog "Call Stack" (List.length lines + 4) w s in
      let s = List.fold_left (fun s (i, l) -> Curses.put ~attrs:grey (top + 2 + i) (left + 3) l s) s (List.mapi (fun i l -> (i, l)) lines) in
      Curses.cursor None s

let view (m : model) : Curses.t =
  let running_user = match (m.mode, m.session) with Executing, Some s when s.swapped -> Some s | _ -> None in
  match (m.mode, running_user) with
  | _, Some s -> Curses.cursor (if s.typing <> None then Some (Vt.cursor s.user) else None) (vt_screen s.user)
  | (Finished (vt, _) | Showing vt), _ -> Curses.cursor None (vt_screen vt)
  | _ -> (
      let s = Curses.create ~rows:24 ~cols:80 in
      let s = edit_window m s in
      let s = watch_window m s in
      let s = status_line m s in
      let menu_open = match m.mode with Menu (b, _) -> Some b | _ -> None in
      let s = menu_bar menu_open s in
      let editing_cursor = Some (2 + m.row - m.top, 1 + m.col - m.left) in
      match m.mode with
      | Menu (bar, sel) -> Curses.cursor None (dropdown bar sel s)
      | Open_dialog sel ->
          let fs = Turbo_menus.files m in
          let s, top, left = dialog "Open a File" (List.length fs + 5) 36 s in
          let s = Curses.put ~attrs:grey (top + 1) (left + 3) "Files" s in
          let s =
            List.fold_left
              (fun s (i, f) -> Curses.put ~attrs:(if i = sel then attrs ~bold:true Vt.White Vt.Cyan else attrs Vt.Black Vt.Cyan) (top + 2 + i) (left + 3) (Printf.sprintf " %-16s" f) s)
              s
              (List.mapi (fun i f -> (i, f)) fs)
          in
          let s = button (top + 2) (left + 24) " Open " s in
          Curses.cursor None (button (top + 4) (left + 24) "Cancel" s)
      | Input { title; label; text; _ } ->
          let s, top, left = dialog title 8 50 s in
          let s = Curses.put ~attrs:grey (top + 2) (left + 3) label s in
          let s = Curses.put ~attrs:(attrs ~bold:true Vt.White Vt.Blue) (top + 3) (left + 3) (Printf.sprintf " %-42s" text) s in
          let s = button (top + 5) (left + 14) "  OK  " s in
          Curses.cursor (Some (top + 3, left + 4 + String.length text)) (button (top + 5) (left + 26) "Cancel" s)
      | Info (title, lines) ->
          let w = max 32 (4 + List.fold_left (fun w l -> max w (String.length l)) 0 lines) in
          let s, top, left = dialog title (List.length lines + 5) w s in
          let s = List.fold_left (fun s (i, l) -> Curses.put ~attrs:grey (top + 2 + i) (left + ((w - String.length l) / 2)) l s) s (List.mapi (fun i l -> (i, l)) lines) in
          Curses.cursor None (button (top + List.length lines + 3) (left + (w / 2) - 4) "  OK  " s)
      | Listing first -> Curses.cursor None (listing m first s)
      | Stack -> stack_window m s
      | Executing -> Curses.cursor None s
      | _ -> Curses.cursor editing_cursor s)
