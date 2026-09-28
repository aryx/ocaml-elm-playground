(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Fli.mli *)

type format = Fli | Flc
type header = { format : format; width : int; height : int; frames : int; delay : float }

(* the chunks' types *)
let color_256 = 4
let delta_flc = 7
let color_64 = 11
let lc = 12
let black = 13
let brun = 15
let copy = 16
let frame_magic = 0xF1FA

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

let u8 (s : string) (i : int) : int = Char.code s.[i]
let u16 (s : string) (i : int) : int = u8 s i lor (u8 s (i + 1) lsl 8)
let u32 (s : string) (i : int) : int = u16 s i lor (u16 s (i + 2) lsl 16)
let signed (v : int) : int = if v >= 128 then v - 256 else v

(* the decoder's state after a frame: the palette (r g b, 256 of them)
 * and the pixels (a palette index each), and the frame next *)
type state = { palette : Bytes.t; pixels : Bytes.t; next_frame : int }

(* one chunk, at [at], applied to the palette and the pixels *)
let apply (h : header) (s : string) (palette : Bytes.t) (pixels : Bytes.t) (typ : int) (at : int) : unit =
  let w = h.width in
  let body = at + 6 in
  let set x y v = Bytes.set pixels ((y * w) + x) (Char.chr v) in
  let blit_from p x y n = Bytes.blit_string s p pixels ((y * w) + x) n in
  if typ = color_256 || typ = color_64 then (
    let p = ref (body + 2) and entry = ref 0 in
    for _ = 1 to u16 s body do
      entry := !entry + u8 s !p;
      let n = match u8 s (!p + 1) with 0 -> 256 | n -> n in
      p := !p + 2;
      for _ = 1 to n do
        for c = 0 to 2 do
          let v = u8 s (!p + c) in
          (* 0-63, the VGA's: stretched to 0-255, its top bits repeated *)
          let v = if typ = color_64 then (v lsl 2) lor (v lsr 4) else v in
          Bytes.set palette ((3 * !entry) + c) (Char.chr (v land 0xFF))
        done;
        p := !p + 3;
        incr entry
      done
    done)
  else if typ = brun then (
    let p = ref body in
    for y = 0 to h.height - 1 do
      incr p (* the packets' count: the line's width says when it ends *);
      let x = ref 0 in
      while !x < w do
        let n = signed (u8 s !p) in
        incr p;
        if n > 0 then (
          for i = 0 to n - 1 do set (!x + i) y (u8 s !p) done;
          incr p;
          x := !x + n)
        else (
          blit_from !p !x y (-n);
          p := !p - n;
          x := !x - n)
      done
    done)
  else if typ = lc then (
    let y = ref (u16 s body) and p = ref (body + 4) in
    for _ = 1 to u16 s (body + 2) do
      let x = ref 0 and packets = u8 s !p in
      incr p;
      for _ = 1 to packets do
        x := !x + u8 s !p;
        let n = signed (u8 s (!p + 1)) in
        p := !p + 2;
        if n > 0 then (
          blit_from !p !x !y n;
          p := !p + n;
          x := !x + n)
        else (
          for i = 0 to -n - 1 do set (!x + i) !y (u8 s !p) done;
          incr p;
          x := !x - n)
      done;
      incr y
    done)
  else if typ = delta_flc then (
    let y = ref 0 and p = ref (body + 2) in
    for _ = 1 to u16 s body do
      (* the opcodes before the line's packets: lines skipped, an odd
       * line's last pixel, and then the packets' count *)
      let rec packets () =
        let word = u16 s !p in
        p := !p + 2;
        if word land 0xC000 = 0xC000 then (
          y := !y + (0x10000 - word);
          packets ())
        else if word land 0xC000 = 0x8000 then (
          set (w - 1) !y (word land 0xFF);
          packets ())
        else word
      in
      let x = ref 0 in
      for _ = 1 to packets () do
        x := !x + u8 s !p;
        let n = signed (u8 s (!p + 1)) in
        p := !p + 2;
        if n > 0 then (
          blit_from !p !x !y (2 * n);
          p := !p + (2 * n);
          x := !x + (2 * n))
        else (
          for i = 0 to -n - 1 do
            set (!x + (2 * i)) !y (u8 s !p);
            set (!x + (2 * i) + 1) !y (u8 s (!p + 1))
          done;
          p := !p + 2;
          x := !x - (2 * n))
      done;
      incr y
    done)
  else if typ = black then Bytes.fill pixels 0 (Bytes.length pixels) '\000'
  else if typ = copy then Bytes.blit_string s body pixels 0 (Bytes.length pixels)

let of_string (s : string) : header * Movie.t =
  if String.length s < 128 then failwith "FLI: too short";
  let format = match u16 s 4 with 0xAF11 -> Fli | 0xAF12 -> Flc | _ -> failwith "FLI: not an FLI or FLC file" in
  let frames = u16 s 6 in
  let width = match u16 s 8 with 0 -> 320 | w -> w and height = match u16 s 10 with 0 -> 200 | h -> h in
  let delay = match format with Fli -> float_of_int (u16 s 16) /. 70. | Flc -> float_of_int (u32 s 16) /. 1000. in
  let h = { format; width; height; frames; delay = Float.max 0.01 delay } in
  if frames = 0 then failwith "FLI: no frames";
  (* where each frame is: the frame chunks, other chunks (an FLC's
   * prefix, 0xF100) skipped, the ring frame after the last not read *)
  let first = match format with Flc when u32 s 80 >= 128 && u32 s 80 < String.length s -> u32 s 80 | _ -> 128 in
  let rec walk at acc n =
    if n = frames || at + 16 > String.length s then List.rev acc
    else
      let size = u32 s at in
      if size < 16 then failwith "FLI: a chunk of no size"
      else if u16 s (at + 4) = frame_magic then walk (at + size) (at :: acc) (n + 1)
      else walk (at + size) acc n
  in
  let starts = Array.of_list (walk first [] 0) in
  if starts = [||] then failwith "FLI: no frames";
  (* an FLC frame can have a delay of its own *)
  let delays = Array.map (fun at -> match format with Flc when u16 s (at + 8) > 0 -> float_of_int (u16 s (at + 8)) /. 1000. | _ -> h.delay) starts in
  let times = Array.make (Array.length starts) 0. in
  for i = 1 to Array.length starts - 1 do times.(i) <- times.(i - 1) +. delays.(i - 1) done;
  let duration = times.(Array.length starts - 1) +. delays.(Array.length starts - 1) in
  let start () = { palette = Bytes.make 768 '\000'; pixels = Bytes.make (width * height) '\000'; next_frame = 0 } in
  let next (st : state) : state * Rgba_image.t =
    (* the frame before, changed: copies, the state before kept intact *)
    let palette = Bytes.copy st.palette and pixels = Bytes.copy st.pixels in
    let at = starts.(st.next_frame) in
    let chunk = ref (at + 16) in
    (try
       for _ = 1 to u16 s (at + 6) do
         apply h s palette pixels (u16 s (!chunk + 4)) !chunk;
         chunk := !chunk + u32 s !chunk
       done
     with Invalid_argument _ -> () (* a chunk cut short: the frame as far as it went *));
    let img = Rgba_image.create ~width ~height in
    for i = 0 to (width * height) - 1 do
      let c = 3 * Char.code (Bytes.get pixels i) in
      for k = 0 to 2 do img.rgba.{(4 * i) + k} <- Char.code (Bytes.get palette (c + k)) done;
      img.rgba.{(4 * i) + 3} <- 255
    done;
    ({ palette; pixels; next_frame = st.next_frame + 1 }, img)
  in
  (h, Movie.sequential ~width ~height ~times ~duration ~start ~next)

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

(* the length of the run of equal values from [x], at most [limit] *)
let run_length (get : int -> int) (x : int) (limit : int) : int =
  let n = ref 1 in
  while !n < limit && get (x + !n) = get x do incr n done;
  !n

(* the values [x, e) as packets, each a count byte and its values:
 * a run of 2 or more as one value repeated, else literals up to where a
 * run of 3 starts; [repeat_sign] -1 when a run's count is negative
 * (LC, DELTA_FLC), 1 when the literals' are (BRUN); [prefix] a byte
 * before each packet (LC's and DELTA_FLC's column skip), [emit] one
 * value's bytes. Gives the packets' count. *)
let packets (b : Buffer.t) ~(repeat_sign : int) ~(prefix : int -> int option) ~(emit : int -> unit) (get : int -> int) (x : int) (e : int) : int =
  let count = ref 0 and x = ref x in
  let add_prefix () = Option.iter (fun v -> Buffer.add_uint8 b v) (prefix !count) in
  while !x < e do
    let r = run_length get !x (min 127 (e - !x)) in
    add_prefix ();
    if r >= 2 then (
      Buffer.add_uint8 b ((repeat_sign * r) land 0xFF);
      emit (get !x);
      x := !x + r)
    else (
      let n = ref 1 in
      while !x + !n < e && !n < 127 && run_length get (!x + !n) (min 3 (e - !x - !n)) < 3 do incr n done;
      Buffer.add_uint8 b ((- repeat_sign * !n) land 0xFF);
      for i = 0 to !n - 1 do emit (get (!x + i)) done;
      x := !x + !n);
    incr count
  done;
  !count

(* BRUN: every line, runs and literals *)
let brun_chunk (w : int) (h : int) (cur : Bytes.t) : Buffer.t =
  let b = Buffer.create (w * h / 2) in
  for y = 0 to h - 1 do
    let line = Buffer.create w in
    let get x = Char.code (Bytes.get cur ((y * w) + x)) in
    let n = packets line ~repeat_sign:1 ~prefix:(fun _ -> None) ~emit:(Buffer.add_uint8 line) get 0 w in
    Buffer.add_uint8 b (min n 255);
    Buffer.add_buffer b line
  done;
  b

(* the changed stretches of a line, [unit] pixels at a time (1: LC's
 * bytes, 2: DELTA_FLC's words, [get x] the value at column x), as
 * packets each preceded by its columns skipped -- a byte: a skip
 * longer than 254 is a packet copying one value unchanged, after 254 *)
let changed_packets (b : Buffer.t) ~(unit : int) ~(width : int) (changed : int -> bool) (get : int -> int) ~(emit : int -> unit) : int =
  let count = ref 0 and x = ref 0 in
  let stop = width - (width mod unit) in
  while !x < stop do
    let x' = ref !x in
    while !x' < stop && not (changed !x') do x' := !x' + unit done;
    if !x' < stop then (
      while !x' - !x > 254 do
        Buffer.add_uint8 b 254;
        Buffer.add_uint8 b 1;
        emit (get (!x + 254));
        incr count;
        x := !x + 254 + unit
      done;
      let e = ref !x' in
      while !e < stop && changed !e do e := !e + unit done;
      let skip = !x' - !x in
      let n =
        packets b ~repeat_sign:(-1) ~prefix:(fun k -> Some (if k = 0 then skip else 0)) ~emit
          (fun k -> get (!x' + (k * unit))) 0 ((!e - !x') / unit)
      in
      count := !count + n;
      x := !e)
    else x := stop
  done;
  !count

(* LC: from the first changed line to the last *)
let lc_chunk (w : int) (h : int) (prev : Bytes.t) (cur : Bytes.t) : Buffer.t option =
  let at x y = Char.code (Bytes.get cur ((y * w) + x)) in
  let changed x y = Bytes.get cur ((y * w) + x) <> Bytes.get prev ((y * w) + x) in
  let line_changed y = List.exists (fun x -> changed x y) (List.init w Fun.id) in
  let lines = List.filter line_changed (List.init h Fun.id) in
  match lines with
  | [] -> None
  | y0 :: _ ->
      let y1 = List.fold_left max y0 lines in
      let b = Buffer.create 1024 in
      Buffer.add_uint16_le b y0;
      Buffer.add_uint16_le b (y1 - y0 + 1);
      for y = y0 to y1 do
        let line = Buffer.create w in
        let n = changed_packets line ~unit:1 ~width:w (fun x -> changed x y) (fun x -> at x y) ~emit:(Buffer.add_uint8 line) in
        if n > 255 then invalid_arg "Fli.to_string: a line of more than 255 packets";
        Buffer.add_uint8 b n;
        Buffer.add_buffer b line
      done;
      Some b

(* DELTA_FLC: the changed lines, by words, the unchanged ones skipped *)
let delta_flc_chunk (w : int) (h : int) (prev : Bytes.t) (cur : Bytes.t) : Buffer.t option =
  let at x y = Char.code (Bytes.get cur ((y * w) + x)) in
  let pixel_changed x y = Bytes.get cur ((y * w) + x) <> Bytes.get prev ((y * w) + x) in
  let b = Buffer.create 1024 and lines = ref 0 and skipped = ref 0 in
  for y = 0 to h - 1 do
    if not (List.exists (fun x -> pixel_changed x y) (List.init w Fun.id)) then incr skipped
    else (
      incr lines;
      if !skipped > 0 then Buffer.add_uint16_le b ((- !skipped) land 0xFFFF);
      skipped := 0;
      if w mod 2 = 1 && pixel_changed (w - 1) y then Buffer.add_uint16_le b (0x8000 lor at (w - 1) y);
      let line = Buffer.create w in
      let changed x = pixel_changed x y || pixel_changed (x + 1) y in
      let word x = at x y lor (at (x + 1) y lsl 8) in
      let n = changed_packets line ~unit:2 ~width:w changed word ~emit:(Buffer.add_uint16_le line) in
      Buffer.add_uint16_le b n;
      Buffer.add_buffer b line)
  done;
  if !lines = 0 then None
  else
    let chunk = Buffer.create (Buffer.length b + 2) in
    Buffer.add_uint16_le chunk !lines;
    Buffer.add_buffer chunk b;
    Some chunk

(* a chunk: its size, its type, its body padded to an even size *)
let add_chunk (b : Buffer.t) (typ : int) (body : Buffer.t) : unit =
  let pad = Buffer.length body mod 2 in
  Buffer.add_int32_le b (Int32.of_int (6 + Buffer.length body + pad));
  Buffer.add_uint16_le b typ;
  Buffer.add_buffer b body;
  if pad = 1 then Buffer.add_uint8 b 0

let add_frame (b : Buffer.t) (chunks : (int * Buffer.t) list) : unit =
  let body = Buffer.create 4096 in
  List.iter (fun (typ, c) -> add_chunk body typ c) chunks;
  Buffer.add_int32_le b (Int32.of_int (16 + Buffer.length body));
  Buffer.add_uint16_le b frame_magic;
  Buffer.add_uint16_le b (List.length chunks);
  Buffer.add_string b (String.make 8 '\000');
  Buffer.add_buffer b body

let to_string ?(format = Flc) ~(delay : float) (frames : Rgba_image.t list) : string =
  let first = match frames with [] -> invalid_arg "Fli.to_string: no frames" | f :: _ -> f in
  let w = first.width and h = first.height in
  (* the palette: every color met, in the order met *)
  let colors = Hashtbl.create 256 and palette = ref [] in
  let indexed (img : Rgba_image.t) : Bytes.t =
    if img.width <> w || img.height <> h then invalid_arg "Fli.to_string: frames of different sizes";
    Bytes.init (w * h) (fun i ->
        let c = (img.rgba.{4 * i}, img.rgba.{(4 * i) + 1}, img.rgba.{(4 * i) + 2}) in
        match Hashtbl.find_opt colors c with
        | Some k -> Char.chr k
        | None ->
            let k = Hashtbl.length colors in
            if k = 256 then invalid_arg "Fli.to_string: more than 256 colors";
            Hashtbl.add colors c k;
            palette := c :: !palette;
            Char.chr k)
  in
  let pixels = List.map indexed frames in
  let color_chunk =
    let b = Buffer.create 770 in
    Buffer.add_uint16_le b 1;
    Buffer.add_uint8 b 0;
    Buffer.add_uint8 b 0 (* 256 entries: the unused ones black *);
    let entries = Array.make 256 (0, 0, 0) in
    List.iteri (fun k c -> entries.(k) <- c) (List.rev !palette);
    Array.iter
      (fun (r, g, bl) ->
        List.iter (fun v -> Buffer.add_uint8 b (match format with Flc -> v | Fli -> v lsr 2)) [ r; g; bl ])
      entries;
    ((match format with Flc -> color_256 | Fli -> color_64), b)
  in
  let body = Buffer.create 65536 in
  let frame_starts = ref [] in
  List.iteri
    (fun i cur ->
      frame_starts := (128 + Buffer.length body) :: !frame_starts;
      if i = 0 then add_frame body [ color_chunk; (brun, brun_chunk w h cur) ]
      else
        let prev = List.nth pixels (i - 1) in
        let delta = match format with Flc -> Option.map (fun c -> (delta_flc, c)) (delta_flc_chunk w h prev cur) | Fli -> Option.map (fun c -> (lc, c)) (lc_chunk w h prev cur) in
        add_frame body (Option.to_list delta))
    pixels;
  let header = Bytes.make 128 '\000' in
  let total = 128 + Buffer.length body in
  Bytes.set_int32_le header 0 (Int32.of_int total);
  Bytes.set_uint16_le header 4 (match format with Fli -> 0xAF11 | Flc -> 0xAF12);
  Bytes.set_uint16_le header 6 (List.length frames);
  Bytes.set_uint16_le header 8 w;
  Bytes.set_uint16_le header 10 h;
  Bytes.set_uint16_le header 12 8;
  (match format with
  | Fli -> Bytes.set_uint16_le header 16 (max 1 (int_of_float (Float.round (delay *. 70.))))
  | Flc ->
      Bytes.set_uint16_le header 14 3;
      Bytes.set_int32_le header 16 (Int32.of_int (int_of_float (Float.round (delay *. 1000.))));
      let starts = Array.of_list (List.rev !frame_starts) in
      Bytes.set_int32_le header 80 (Int32.of_int starts.(0));
      if Array.length starts > 1 then Bytes.set_int32_le header 84 (Int32.of_int starts.(1)));
  Bytes.to_string header ^ Buffer.contents body
