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
open Basics (* float arithmetics *)

type look = {
  keys : int;
  left : number;
  top : number;
  white_width : number;
  white_height : number;
  black_height : number;
  letters_from : int;
  velocity : number;
  octaves : int * int;
  white_key : color;
  black_key : color;
  letter_on_white : color;
  letter_scale : number;
  letter_lift : number;
}

type t = {
  octave : int;
  held : string list; (* the letters held at the last frame *)
  mouse_note : int option; (* the key the mouse holds down *)
  on_keys : bool; (* the mouse was pressed on a key *)
  was_down : bool;
}

let initial ~octave : t = { octave; held = []; mouse_note = None; on_keys = false; was_down = false }
let octave (t : t) : int = t.octave

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

let is_black (s : int) : bool = List.mem (s mod 12) [ 1; 3; 6; 8; 10 ]
let whites_before (s : int) : int = List.length (List.filter (fun i -> not (is_black i)) (List.init s (fun i -> i)))
let note (look : look) (t : t) (s : int) : int = (12 *.. (t.octave +.. 1)) +.. s -.. look.letters_from

let key_x (look : look) (s : int) : number =
  let w = float_of_int (whites_before s) in
  if is_black s then look.left + (w * look.white_width) else look.left + ((w + 0.5) * look.white_width)

let key_at (look : look) (x : number) (y : number) : int option =
  let keys = List.init look.keys (fun s -> s) in
  let hit s =
    let w = if is_black s then look.white_width * 0.6 else look.white_width in
    let h = if is_black s then look.black_height else look.white_height in
    Float.abs (x - key_x look s) <= w / 2. && y <= look.top && y >= look.top - h
  in
  match List.find_opt (fun s -> is_black s && hit s) keys with Some s -> Some s | None -> List.find_opt hit keys

let update (look : look) (computer : computer) (t : t) (inst : Instrument.t) : t =
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k t.held) and released k = List.mem k t.held && not (List.mem k now) in
  let lowest, highest = look.octaves in
  let octave = if pressed "z" then max lowest (t.octave -.. 1) else if pressed "x" then min highest (t.octave +.. 1) else t.octave in
  List.iter
    (fun (k, semitone) ->
      let n = (12 *.. (t.octave +.. 1)) +.. semitone in
      if pressed k then inst.note_on n look.velocity;
      if released k then inst.note_off n)
    letters;
  let mouse = computer.mouse in
  let on_keys = if mouse.mdown && not t.was_down then key_at look mouse.mx mouse.my <> None else mouse.mdown && t.on_keys in
  let under = if on_keys then Option.map (note look t) (key_at look mouse.mx mouse.my) else None in
  if under <> t.mouse_note then begin
    Option.iter inst.note_off t.mouse_note;
    Option.iter (fun n -> inst.note_on n look.velocity) under
  end;
  { octave; held = now; mouse_note = under; on_keys; was_down = mouse.mdown }

let view (look : look) (computer : computer) (t : t) ~(lit : color) : shape list =
  let letter_of s = List.find_map (fun (k, s') -> if s' = s -.. look.letters_from then Some k else None) letters in
  let down s = t.mouse_note = Some (note look t s) || match letter_of s with Some k -> Set_.mem k computer.keyboard.keys | None -> false in
  let key s =
    let black = is_black s in
    let w = if black then look.white_width * 0.6 else look.white_width - 3. in
    let h = if black then look.black_height else look.white_height in
    let color = if down s then lit else if black then look.black_key else look.white_key in
    let label =
      match letter_of s with
      | Some k -> [ words (if black then white else look.letter_on_white) k |> scale look.letter_scale |> move_y ((-.h / 2.) + look.letter_lift) ]
      | None -> []
    in
    group (rectangle color w h :: label) |> move (key_x look s) (look.top - (h / 2.))
  in
  let keys = List.init look.keys (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys) @ List.map key (List.filter is_black keys)
