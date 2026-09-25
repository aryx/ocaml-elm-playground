(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tty_unix.mli *)

let write (s : string) : unit =
  let rec go i = if i < String.length s then go (i + Unix.write_substring Unix.stdout s i (String.length s - i)) in
  go 0

(* the bytes waiting on stdin, none if none *)
let available () : string =
  let buf = Bytes.create 256 in
  match Unix.read Unix.stdin buf 0 256 with n -> Bytes.sub_string buf 0 n | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ""

let run (_caps : < Cap.stdin ; Cap.stdout ; .. >) (p : 'model Tui.program) : unit =
  let saved = Unix.tcgetattr Unix.stdin in
  (* raw: keys at once, unechoed, Control-C a key; a read returning
     what is there, maybe nothing *)
  Unix.tcsetattr Unix.stdin Unix.TCSANOW
    { saved with c_icanon = false; c_echo = false; c_isig = false; c_ixon = false; c_icrnl = false; c_vmin = 0; c_vtime = 0 };
  write "\x1b[?1049h";
  Fun.protect
    ~finally:(fun () ->
      write "\x1b[0m\x1b[?25h\x1b[?1049l";
      Unix.tcsetattr Unix.stdin Unix.TCSANOW saved)
    (fun () ->
      let screen = p.view p.init in
      write (Curses.redraw screen);
      let rec loop (model : 'model) (shown : Curses.t) (last : float) =
        if not (p.over model) then begin
          (match Unix.select [ Unix.stdin ] [] [] 0.05 with _ -> () | exception Unix.Unix_error (Unix.EINTR, _, _) -> ());
          let keys = Line_discipline.split_keys (available ()) in
          (* Control-C: the program's end, in raw mode as without *)
          if List.mem "\x03" keys then ()
          else begin
            let model = List.fold_left (fun m k -> p.update (Key k) m) model keys in
            let now = Unix.gettimeofday () in
            (* a pause (the process stopped, Control-Z) isn't time played *)
            let model = p.update (Tick (Float.min 0.25 (now -. last))) model in
            let next = p.view model in
            write (Curses.refresh ~before:shown next);
            loop model next now
          end
        end
      in
      loop p.init screen (Unix.gettimeofday ()))
