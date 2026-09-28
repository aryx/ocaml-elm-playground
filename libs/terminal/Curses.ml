(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Curses.mli *)

(*****************************************************************************)
(* Screens *)
(*****************************************************************************)

(* the rows are shared between versions: drawing on a row copies that
   row only *)
type t = { rows : int; cols : int; cells : Vt.cell array array; cursor : (int * int) option }

let create ~rows ~cols = { rows; cols; cells = Array.make rows (Array.make cols Vt.blank); cursor = None }
let rows (t : t) = t.rows
let cols (t : t) = t.cols
let cursor (c : (int * int) option) (t : t) : t = { t with cursor = c }
let cell (t : t) r c = if r >= 0 && r < t.rows && c >= 0 && c < t.cols then t.cells.(r).(c) else Vt.blank

(* a string's UTF-8 characters, each as its bytes *)
let glyphs (s : string) : string list =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let b = Char.code s.[i] in
      let len = if b land 0xE0 = 0xC0 then 2 else if b land 0xF0 = 0xE0 then 3 else if b land 0xF8 = 0xF0 then 4 else 1 in
      let len = min len (n - i) in
      go (i + len) (String.sub s i len :: acc)
  in
  go 0 []

let put ?(attrs = Vt.plain) (r : int) (c : int) (text : string) (t : t) : t =
  if r < 0 || r >= t.rows then t
  else begin
    let row = Array.copy t.cells.(r) in
    List.iteri (fun i g -> let c = c + i in if c >= 0 && c < t.cols then row.(c) <- { Vt.glyph = g; attrs }) (glyphs text);
    let cells = Array.copy t.cells in
    cells.(r) <- row;
    { t with cells }
  end

let box ?attrs (top : int) (left : int) (height : int) (width : int) (t : t) : t =
  let edge = "+" ^ String.make (max 0 (width - 2)) '-' ^ "+" in
  let t = put ?attrs top left edge t |> put ?attrs (top + height - 1) left edge in
  List.fold_left (fun t r -> t |> put ?attrs r left "|" |> put ?attrs r (left + width - 1) "|") t (List.init (max 0 (height - 2)) (fun i -> top + 1 + i))

let text (t : t) : string list =
  Array.to_list t.cells
  |> List.map (fun row ->
         let s = String.concat "" (Array.to_list (Array.map (fun (c : Vt.cell) -> c.glyph) row)) in
         let n = ref (String.length s) in
         while !n > 0 && s.[!n - 1] = ' ' do decr n done;
         String.sub s 0 !n)

(*****************************************************************************)
(* The difference, as bytes *)
(*****************************************************************************)

let cup (r : int) (c : int) : string = Printf.sprintf "\x1b[%d;%dH" (r + 1) (c + 1)

let sgr (a : Vt.attrs) : string =
  let color base (c : Vt.color) =
    match c with
    | Default -> []
    | Black -> [ base ] | Red -> [ base + 1 ] | Green -> [ base + 2 ] | Yellow -> [ base + 3 ]
    | Blue -> [ base + 4 ] | Magenta -> [ base + 5 ] | Cyan -> [ base + 6 ] | White -> [ base + 7 ]
  in
  let codes = (0 :: (if a.bold then [ 1 ] else [])) @ (if a.reverse then [ 7 ] else []) @ color 30 a.fg @ color 40 a.bg in
  "\x1b[" ^ String.concat ";" (List.map string_of_int codes) ^ "m"

let refresh ~(before : t) (after : t) : string =
  let b = Buffer.create 256 in
  (* where the terminal's cursor is: where the last refresh left it,
     None when unknown (hidden, or after the last column: the wrap is
     pending); and its colours, plain between refreshes *)
  let at = ref before.cursor and pen = ref Vt.plain in
  let send (r : int) (c : int) (cell : Vt.cell) =
    if cell.attrs <> !pen then begin
      Buffer.add_string b (sgr cell.attrs);
      pen := cell.attrs
    end;
    Buffer.add_string b cell.glyph;
    at := if c + 1 < after.cols then Some (r, c + 1) else None
  in
  for r = 0 to after.rows - 1 do
    (* a row shared by the two versions hasn't changed *)
    if r >= before.rows || before.cells.(r) != after.cells.(r) then
      for c = 0 to after.cols - 1 do
        let now = after.cells.(r).(c) in
        if cell before r c <> now then begin
          (match !at with
          | Some (r', c') when r' = r && c' = c -> ()
          (* the gap sent again, when cheaper than the move and in the
             colours already set *)
          | Some (r', c')
            when r' = r && c' < c
                 && c - c' < String.length (cup r c)
                 && List.for_all (fun i -> after.cells.(r).(i).attrs = !pen) (List.init (c - c') (fun i -> c' + i)) ->
              for i = c' to c - 1 do
                send r i after.cells.(r).(i)
              done
          | _ -> Buffer.add_string b (cup r c));
          send r c now
        end
      done
  done;
  if !pen <> Vt.plain then Buffer.add_string b (sgr Vt.plain);
  (match after.cursor with
  | Some (r, c) ->
      if !at <> Some (r, c) then Buffer.add_string b (cup r c);
      if before.cursor = None then Buffer.add_string b "\x1b[?25h"
  | None -> if before.cursor <> None then Buffer.add_string b "\x1b[?25l");
  Buffer.contents b

let redraw (t : t) : string =
  let blank = { (create ~rows:t.rows ~cols:t.cols) with cursor = Some (0, 0) } in
  (* the terminal's cursor shown, so that a hidden one is hidden *)
  "\x1b[0m\x1b[2J\x1b[H\x1b[?25h" ^ refresh ~before:blank t
