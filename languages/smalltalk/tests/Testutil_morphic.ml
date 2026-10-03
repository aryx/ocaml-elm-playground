(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Testutil_morphic.mli *)

module I = St_interp

type world = { vm : I.vm; mouse : (int * int * int) ref; keys : int Queue.t }

let print (w : world) (text : string) : string =
  match I.evaluate w.vm ~budget:50_000_000 text with
  | Ok v ->
      let s = I.print_string w.vm v in
      let n = String.length s in
      if n >= 2 && s.[0] = '\'' && s.[n - 1] = '\'' then String.sub s 1 (n - 2) else s
  | Error e -> "error: " ^ e

let boot ?(size = (400, 300)) () : world =
  let mouse = ref (0, 0, 0) and keys = Queue.create () in
  let host = { St_boot.quiet_host with mouse = (fun () -> !mouse); keyboard = (fun () -> Queue.take_opt keys) } in
  let w = { vm = St_boot.boot ~host ~kernel:St_kernel.squeak (); mouse; keys } in
  Alcotest.(check string)
    "a world" "a PasteUpMorph"
    (print w
       (Printf.sprintf
          "Smalltalk at: #F put: (Form extent: %d @ %d depth: 32). Smalltalk at: #W put: (PasteUpMorph on: F). W doOneCycle. W"
          (fst size) (snd size)));
  w

let set_mouse (w : world) (x : int) (y : int) (buttons : int) : unit = w.mouse := (x, y, buttons)

let move (w : world) (x : int) (y : int) (buttons : int) : unit =
  set_mouse w x y buttons;
  ignore (print w "W doOneCycle")

let click ?(button = 4) (w : world) (x : int) (y : int) : unit =
  move w x y button;
  move w x y 0

let typed (w : world) (s : string) : unit =
  String.iter (fun c -> Queue.add (Char.code c) w.keys) s;
  ignore (print w "W doOneCycle")

let colour (w : world) (x : int) (y : int) : string = print w (Printf.sprintf "F colorAt: %d @ %d" x y)
