(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Vt.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type color = Default | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
type attrs = { fg : color; bg : color; bold : bool; reverse : bool }
type cell = { glyph : string; attrs : attrs }

let plain = { fg = Default; bg = Default; bold = false; reverse = false }
let blank = { glyph = " "; attrs = plain }

(* where the parser is in a sequence (Vt.mli's diagram) *)
type state =
  | Ground
  | Escape
  (* ESC ( B, ESC # 8: one more byte, then done *)
  | Escape_skip
  (* after ESC [: the parameters (and intermediates) so far *)
  | Csi of string
  | Osc
  (* an ESC inside an OSC: the start of its ESC \ ending *)
  | Osc_escape
  (* a UTF-8 character: its bytes so far, how many still to come *)
  | Utf8 of string * int

(* mutable, but only ever a copy: [feed] copies, then changes the copy *)
type t = {
  rows : int;
  cols : int;
  cells : cell array array;
  mutable row : int;
  mutable col : int;
  mutable wrap_pending : bool;
  mutable attrs : attrs;
  (* the scrolling region, rows from 0, both included *)
  mutable top : int;
  mutable bottom : int;
  mutable saved : int * int * attrs;
  mutable cursor_visible : bool;
  mutable bells : int;
  mutable state : state;
}

let create ~rows ~cols =
  {
    rows;
    cols;
    cells = Array.init rows (fun _ -> Array.make cols blank);
    row = 0;
    col = 0;
    wrap_pending = false;
    attrs = plain;
    top = 0;
    bottom = rows - 1;
    saved = (0, 0, plain);
    cursor_visible = true;
    bells = 0;
    state = Ground;
  }

let copy (t : t) : t = { t with cells = Array.map Array.copy t.cells }

(*****************************************************************************)
(* The cursor *)
(*****************************************************************************)

let clamp lo hi x = max lo (min hi x)

(* every move but writing a character forgets a pending wrap *)
let move_to (t : t) (row : int) (col : int) : unit =
  t.row <- clamp 0 (t.rows - 1) row;
  t.col <- clamp 0 (t.cols - 1) col;
  t.wrap_pending <- false

(*****************************************************************************)
(* Scrolling and erasing *)
(*****************************************************************************)

(* the rows [top..bottom] move up by [n], blank rows coming in at the
   bottom: what LF does on the region's last row *)
let scroll_up (t : t) ~top ~bottom (n : int) : unit =
  let n = min n (bottom - top + 1) in
  for r = top to bottom do
    t.cells.(r) <- (if r + n <= bottom then t.cells.(r + n) else Array.make t.cols blank)
  done

(* the other way, blank rows coming in at the top: ESC M on the
   region's first row, and CSI L *)
let scroll_down (t : t) ~top ~bottom (n : int) : unit =
  let n = min n (bottom - top + 1) in
  for r = bottom downto top do
    t.cells.(r) <- (if r - n >= top then t.cells.(r - n) else Array.make t.cols blank)
  done

let line_feed (t : t) : unit =
  if t.row = t.bottom then scroll_up t ~top:t.top ~bottom:t.bottom 1
  else if t.row < t.rows - 1 then t.row <- t.row + 1;
  t.wrap_pending <- false

let reverse_index (t : t) : unit =
  if t.row = t.top then scroll_down t ~top:t.top ~bottom:t.bottom 1 else if t.row > 0 then t.row <- t.row - 1;
  t.wrap_pending <- false

(* columns [c1..c2] of a row, blanked *)
let erase (t : t) (row : int) (c1 : int) (c2 : int) : unit =
  for c = max 0 c1 to min (t.cols - 1) c2 do
    t.cells.(row).(c) <- blank
  done

(*****************************************************************************)
(* Characters *)
(*****************************************************************************)

(* the deferred wrap: writing in the last column only raises the flag,
   and the wrap happens when the next character comes (Vt.mli) *)
let put (t : t) (glyph : string) : unit =
  if t.wrap_pending then begin
    t.col <- 0;
    line_feed t
  end;
  t.cells.(t.row).(t.col) <- { glyph; attrs = t.attrs };
  if t.col = t.cols - 1 then t.wrap_pending <- true else t.col <- t.col + 1

let replacement = "\xEF\xBF\xBD" (* U+FFFD *)

(* the controls, done at once in any state *)
let control (t : t) (c : char) : unit =
  match c with
  | '\x07' -> t.bells <- t.bells + 1
  | '\b' -> move_to t t.row (t.col - 1)
  | '\t' -> move_to t t.row ((t.col / 8 + 1) * 8)
  | '\n' | '\x0B' | '\x0C' -> line_feed t
  | '\r' -> move_to t t.row 0
  | _ -> ()

(*****************************************************************************)
(* SGR: how the next characters look *)
(*****************************************************************************)

let color_of (n : int) : color =
  match n with
  | 0 -> Black
  | 1 -> Red
  | 2 -> Green
  | 3 -> Yellow
  | 4 -> Blue
  | 5 -> Magenta
  | 6 -> Cyan
  | _ -> White

let rec sgr (a : attrs) (ps : int list) : attrs =
  match ps with
  | [] -> a
  | 0 :: rest -> sgr plain rest
  | 1 :: rest -> sgr { a with bold = true } rest
  | 22 :: rest -> sgr { a with bold = false } rest
  | 7 :: rest -> sgr { a with reverse = true } rest
  | 27 :: rest -> sgr { a with reverse = false } rest
  | n :: rest when n >= 30 && n <= 37 -> sgr { a with fg = color_of (n - 30) } rest
  | 39 :: rest -> sgr { a with fg = Default } rest
  | n :: rest when n >= 40 && n <= 47 -> sgr { a with bg = color_of (n - 40) } rest
  | 49 :: rest -> sgr { a with bg = Default } rest
  (* aixterm's bright colours: the colour, bold *)
  | n :: rest when n >= 90 && n <= 97 -> sgr { a with fg = color_of (n - 90); bold = true } rest
  | n :: rest when n >= 100 && n <= 107 -> sgr { a with bg = color_of (n - 100) } rest
  (* 256 colours and 24-bit ones: their arguments skipped, ignored *)
  | (38 | 48) :: 5 :: _ :: rest -> sgr a rest
  | (38 | 48) :: 2 :: _ :: _ :: _ :: rest -> sgr a rest
  | _ :: rest -> sgr a rest

(*****************************************************************************)
(* CSI: the sequences with numbers *)
(*****************************************************************************)

(* "1;31" -> [1; 31], an empty parameter being 0 ("" -> [0], ";5" -> [0; 5]) *)
let params (s : string) : int list = String.split_on_char ';' s |> List.map (fun p -> Option.value (int_of_string_opt p) ~default:0)

(* the i-th parameter, 0 or missing meaning [default] *)
let nth (ps : int list) (i : int) (default : int) : int =
  match List.nth_opt ps i with
  | Some n when n > 0 -> n
  | _ -> default

let csi (t : t) (collected : string) (final : char) : unit =
  let private_ = String.length collected > 0 && collected.[0] = '?' in
  let ps = params (if private_ then String.sub collected 1 (String.length collected - 1) else collected) in
  let n = nth ps 0 1 in
  let mode = match ps with m :: _ -> m | [] -> 0 in
  match final with
  | _ when String.exists (fun c -> c >= ' ' && c <= '/') collected -> () (* intermediates: none we know *)
  | 'h' | 'l' -> if private_ && mode = 25 then t.cursor_visible <- final = 'h'
  | _ when private_ -> ()
  | 'A' -> move_to t (t.row - n) t.col
  | 'B' -> move_to t (t.row + n) t.col
  | 'C' -> move_to t t.row (t.col + n)
  | 'D' -> move_to t t.row (t.col - n)
  | 'E' -> move_to t (t.row + n) 0
  | 'F' -> move_to t (t.row - n) 0
  | 'G' -> move_to t t.row (n - 1)
  | 'd' -> move_to t (n - 1) t.col
  | 'H' | 'f' -> move_to t (nth ps 0 1 - 1) (nth ps 1 1 - 1)
  | 'J' ->
      let before () = for r = 0 to t.row - 1 do erase t r 0 (t.cols - 1) done in
      let after () = for r = t.row + 1 to t.rows - 1 do erase t r 0 (t.cols - 1) done in
      (match mode with
      | 0 -> erase t t.row t.col (t.cols - 1); after ()
      | 1 -> before (); erase t t.row 0 t.col
      | _ -> before (); erase t t.row 0 (t.cols - 1); after ())
  | 'K' -> (
      match mode with
      | 0 -> erase t t.row t.col (t.cols - 1)
      | 1 -> erase t t.row 0 t.col
      | _ -> erase t t.row 0 (t.cols - 1))
  | 'X' -> erase t t.row t.col (t.col + n - 1)
  (* lines inserted and deleted at the cursor, inside the region *)
  | 'L' -> if t.row >= t.top && t.row <= t.bottom then scroll_down t ~top:t.row ~bottom:t.bottom n
  | 'M' -> if t.row >= t.top && t.row <= t.bottom then scroll_up t ~top:t.row ~bottom:t.bottom n
  (* characters inserted and deleted at the cursor, the rest of the
     line sliding right or left *)
  | '@' ->
      let line = t.cells.(t.row) in
      for c = t.cols - 1 downto t.col do
        line.(c) <- (if c - n >= t.col then line.(c - n) else blank)
      done
  | 'P' ->
      let line = t.cells.(t.row) in
      for c = t.col to t.cols - 1 do
        line.(c) <- (if c + n < t.cols then line.(c + n) else blank)
      done
  | 'm' -> t.attrs <- sgr t.attrs ps
  | 'r' ->
      let top = nth ps 0 1 - 1 and bottom = nth ps 1 t.rows - 1 in
      if top < bottom && bottom < t.rows then begin
        t.top <- top;
        t.bottom <- bottom;
        move_to t 0 0
      end
  | 's' -> t.saved <- (t.row, t.col, t.attrs)
  | 'u' ->
      let row, col, attrs = t.saved in
      move_to t row col;
      t.attrs <- attrs
  | _ -> ()

(*****************************************************************************)
(* The state machine *)
(*****************************************************************************)

let reset (t : t) : unit =
  let fresh = create ~rows:t.rows ~cols:t.cols in
  Array.iteri (fun r row -> t.cells.(r) <- row) fresh.cells;
  move_to t 0 0;
  t.attrs <- plain;
  t.top <- 0;
  t.bottom <- t.rows - 1;
  t.saved <- (0, 0, plain);
  t.cursor_visible <- true

let escape (t : t) (c : char) : state =
  match c with
  | '[' -> Csi ""
  | ']' -> Osc
  | '(' | ')' | '#' -> Escape_skip
  | '7' ->
      t.saved <- (t.row, t.col, t.attrs);
      Ground
  | '8' ->
      let row, col, attrs = t.saved in
      move_to t row col;
      t.attrs <- attrs;
      Ground
  | 'D' ->
      line_feed t;
      Ground
  | 'M' ->
      reverse_index t;
      Ground
  | 'E' ->
      move_to t t.row 0;
      line_feed t;
      Ground
  | 'c' ->
      reset t;
      Ground
  | _ -> Ground

(* how many bytes follow a UTF-8 lead byte; None if it isn't one *)
let utf8_following (c : char) : int option =
  let b = Char.code c in
  if b land 0xE0 = 0xC0 then Some 1 else if b land 0xF0 = 0xE0 then Some 2 else if b land 0xF8 = 0xF0 then Some 3 else None

let rec step (t : t) (c : char) : unit =
  match t.state, c with
  (* in any state: ESC starts over, CAN and SUB abandon *)
  | (Escape | Csi _), '\x1b' -> t.state <- Escape
  | (Escape | Escape_skip | Csi _), ('\x18' | '\x1a') -> t.state <- Ground
  | Ground, '\x1b' -> t.state <- Escape
  | Ground, c when c < ' ' -> control t c
  | Ground, '\x7f' -> ()
  | Ground, c when c < '\x80' -> put t (String.make 1 c)
  | Ground, c -> (
      match utf8_following c with
      | Some n -> t.state <- Utf8 (String.make 1 c, n)
      | None -> put t replacement)
  | Utf8 (bytes, n), c when Char.code c land 0xC0 = 0x80 ->
      let bytes = bytes ^ String.make 1 c in
      if n = 1 then begin
        t.state <- Ground;
        put t bytes
      end
      else t.state <- Utf8 (bytes, n - 1)
  (* a character cut short: shown as U+FFFD, the byte read again *)
  | Utf8 _, c ->
      t.state <- Ground;
      put t replacement;
      step t c
  | (Escape | Escape_skip | Csi _), c when c < ' ' -> control t c
  | Escape, c -> t.state <- escape t c
  | Escape_skip, _ -> t.state <- Ground
  | Csi collected, c when c >= ' ' && c <= '?' -> t.state <- Csi (collected ^ String.make 1 c)
  | Csi collected, c ->
      t.state <- Ground;
      if c >= '@' && c <= '~' then csi t collected c
  | Osc, '\x07' -> t.state <- Ground
  | Osc, '\x1b' -> t.state <- Osc_escape
  | Osc, _ -> ()
  | Osc_escape, _ -> t.state <- Ground

let feed (t : t) (bytes : string) : t =
  let t = copy t in
  String.iter (step t) bytes;
  t

(*****************************************************************************)
(* Reading the screen *)
(*****************************************************************************)

let rows (t : t) = t.rows
let cols (t : t) = t.cols
let cell (t : t) r c = if r >= 0 && r < t.rows && c >= 0 && c < t.cols then t.cells.(r).(c) else blank
let cursor (t : t) = (t.row, t.col)
let cursor_visible (t : t) = t.cursor_visible
let bells (t : t) = t.bells

let rtrim (s : string) : string =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let text (t : t) : string list =
  Array.to_list t.cells |> List.map (fun row -> rtrim (String.concat "" (Array.to_list (Array.map (fun c -> c.glyph) row))))

(*****************************************************************************)
(* The keyboard *)
(*****************************************************************************)

let key ~(ctrl : bool) (name : string) : string option =
  match name with
  | _ when ctrl && String.length name = 1 && Char.lowercase_ascii name.[0] >= 'a' && Char.lowercase_ascii name.[0] <= 'z' ->
      Some (String.make 1 (Char.chr (Char.code (Char.lowercase_ascii name.[0]) - Char.code 'a' + 1)))
  | "Enter" -> Some "\r"
  | "Backspace" -> Some "\x7f"
  | "Tab" -> Some "\t"
  | "Escape" -> Some "\x1b"
  | "ArrowUp" -> Some "\x1b[A"
  | "ArrowDown" -> Some "\x1b[B"
  | "ArrowRight" -> Some "\x1b[C"
  | "ArrowLeft" -> Some "\x1b[D"
  | "Home" -> Some "\x1b[H"
  | "End" -> Some "\x1b[F"
  | "Insert" -> Some "\x1b[2~"
  | "Delete" -> Some "\x1b[3~"
  | "PageUp" -> Some "\x1b[5~"
  | "PageDown" -> Some "\x1b[6~"
  | "F1" -> Some "\x1bOP"
  | "F2" -> Some "\x1bOQ"
  | "F3" -> Some "\x1bOR"
  | "F4" -> Some "\x1bOS"
  | _ -> None
