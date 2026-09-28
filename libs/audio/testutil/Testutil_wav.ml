(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Testutil_wav.mli *)

(* [samples] written to actual/[name].wav and compared, as 16-bit
 * values, with golden/[name].wav: None if they're the same, else what's
 * wrong *)
let compare ~(dir : string) (name : string) (samples : Signal.t) : string option =
  let file = name ^ ".wav" in
  (* the tests run in parallel: another one may have just made it *)
  (try Sys.mkdir "actual" 0o755 with Sys_error _ -> ());
  Wav.write (Filename.concat "actual" file) samples;
  let hint = Printf.sprintf "the new sound is _build/default/%s/actual/%s (approve after listening)" dir file in
  match Wav.read (Filename.concat "golden" file) with
  | exception Sys_error _ -> Some (Printf.sprintf "no golden WAV golden/%s yet; %s" file hint)
  | Error e -> Some (Printf.sprintf "golden/%s: %s" file e)
  | Ok golden ->
      let differ = ref 0 and first = ref (-1) in
      for i = 0 to max (Array.length golden) (Array.length samples) - 1 do
        let a = if i < Array.length golden then Signal.to_int16 golden.(i) else max_int in
        let b = if i < Array.length samples then Signal.to_int16 samples.(i) else min_int in
        if a <> b then (
          if !first < 0 then first := i;
          incr differ)
      done;
      if !differ > 0 then Some (Printf.sprintf "%s: %d samples differ, the first at %d; %s" file !differ !first hint) else None

let fail (errors : string option list) : unit =
  match List.filter_map Fun.id errors with [] -> () | es -> Alcotest.fail (String.concat "\n" es)

let check ~(dir : string) (name : string) (samples : Signal.t) : unit = fail [ compare ~dir name samples ]

(* each channel its own mono file: Wav.read mixes a stereo one down, and
 * the panning is what a stereo golden is for; both written before either
 * fails *)
let check_stereo ~(dir : string) (name : string) (s : Signal.stereo) : unit =
  fail [ compare ~dir (name ^ "_left") s.left; compare ~dir (name ^ "_right") s.right ]
