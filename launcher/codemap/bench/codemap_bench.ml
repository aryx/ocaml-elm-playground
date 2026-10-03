(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The code map of a directory, as tinybox codemap <dir> makes it, its
 * frames timed without a window: the reading, the map made, then frames
 * of update and view (the shapes, not their drawing), clicks at given
 * frames. A frame over 1/30 s is flagged: what makes the map feel
 * frozen.
 *
 *   codemap_bench <dir> [x,y@frame | key@frame ...]
 *
 * e.g. codemap_bench ~/principia -300,150@5 -300,150@40 : a click at
 * frame 5 (the folder there looked at), another at 40 (inside it);
 * x@5, the key x pressed at frame 5. IDLE=n: n more frames, their
 * average. OPTI=off: the simple code instead of the optimized
 * (Opti.mli), to see what each optimization buys. *)

let time f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (r, (Unix.gettimeofday () -. t0) *. 1000.)

let () =
  let dir, clicks =
    match Array.to_list Sys.argv with
    | _ :: dir :: clicks ->
        ( dir,
          List.map (fun c -> match Scanf.sscanf c "%f,%f@%d" (fun x y f -> (f, `Click (x, y))) with r -> r | exception _ -> Scanf.sscanf c "%[^@]@%d" (fun k f -> (f, `Key k))) clicks )
    | _ -> failwith "usage: codemap_bench <dir> [x,y@frame | key@frame ...]"
  in
  let w, ms = time (fun () -> Code_walk.walk dir) in
  Printf.printf "read: %d files, %.0f ms\n%!" (List.length w.sources) ms;
  let guide, _ = Code_guide.load ~read:(fun p -> Code_walk.read (Filename.concat dir p)) w.configs in
  let screen : Playground.screen = { width = 1778.; height = 1000.; left = -889.; right = 889.; top = 500.; bottom = -500. } in
  Code_map.choose_style "atlas";
  (* OPTI=off: the simple code, not the optimized (Opti.mli) *)
  if Sys.getenv_opt "OPTI" = Some "off" then Opti.enabled := false;
  (* RANK=once: the uses counted once and given to every map, as a web
   * page's bundle gives them (Codemap.use_rank) *)
  if Sys.getenv_opt "RANK" = Some "once" then begin
    let files = List.map (fun (p, src) -> (p, lazy (Code_file.make p src))) w.sources in
    let (), lex = time (fun () -> List.iter (fun (_, f) -> ignore (Lazy.force f)) files) in
    Printf.printf "lexed: %.0f ms\n%!" lex;
    let r, ms = time (fun () -> Code_rank.compute ~roots:w.roots files) in
    Printf.printf "rank counted once: %.0f ms\n%!" ms;
    Codemap.use_rank r
  end;
  let c, ms =
    time (fun () ->
        Codemap.of_directory ~guide ~colours:(Code_guide.colours guide) ~roots:w.roots ~area:(screen.left +. 20., screen.top -. 92., 1778 - 40, 1000 - 162) ~name:(Filename.basename dir) ~sources:w.sources ())
  in
  Printf.printf "map made: %.0f ms\n%!" ms;
  (* IDLE=n: n more frames with nothing happening, their average said *)
  let idle = match Sys.getenv_opt "IDLE" with Some n -> int_of_string n | None -> 0 in
  let busy = List.fold_left (fun m (f, _) -> max m f) 0 clicks + 40 in
  let last = busy + idle in
  let idle_ms = ref 0. in
  let c = ref c in
  let mouse = ref (0., 0.) in
  for frame = 0 to last do
    let event = List.assoc_opt frame clicks in
    let click = match event with Some (`Click p) -> Some p | _ -> None in
    let key = match event with Some (`Key k) -> Some k | _ -> None in
    Option.iter (fun p -> mouse := p) click;
    let mx, my = !mouse in
    let computer : Playground.computer =
      {
        Playground.initial_computer with
        screen;
        time = Time (float_of_int frame /. 60.);
        mouse = { Playground.initial_computer.mouse with mx; my; mclick = click <> None };
      }
    in
    let c', up = time (fun () -> Codemap.update computer ~pressed:(fun k -> Some k = key) ~arrow:None !c) in
    (match c' with Some c' -> c := c' | None -> ());
    let shapes, vw = time (fun () -> Codemap.view computer !c) in
    if frame > busy then idle_ms := !idle_ms +. up +. vw;
    if up +. vw > 33. || event <> None || frame < 2 then
      Printf.printf "frame %3d%s: update %6.0f ms, view %6.0f ms, %d shapes%s\n%!" frame
        (match event with Some (`Click _) -> " (click)" | Some (`Key k) -> " (" ^ k ^ ")" | None -> "") up vw (List.length shapes)
        (if up +. vw > 33. then "  SLOW" else "")
  done
  ;
  if idle > 0 then Printf.printf "idle frames: %.1f ms each\n" (!idle_ms /. float_of_int idle)
