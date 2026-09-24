(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mpeg_audio.mli *)

let decode (s : string) : (Mpeg_audio_header.t * Signal.stereo, string) result =
  match Mpeg_audio_header.frames s with
  | [] -> Error "no MPEG audio frame"
  | (_, first) :: _ when first.layer = 1 -> Error "MPEG audio Layer I: not read"
  | (_, first) :: _ as frames ->
      let total = List.fold_left (fun n (_, (h : Mpeg_audio_header.t)) -> n + h.samples) 0 frames in
      let left = Array.make total 0. and right = Array.make total 0. in
      let filters = [| Polyphase.create (); Polyphase.create () |] in
      let bits = Bits.of_string s in
      let layer3 = Layer3.create () in
      let at = ref 0 in
      List.iter
        (fun (pos, (h : Mpeg_audio_header.t)) ->
          Bits.seek bits (8 * (pos + 4 + if h.crc then 2 else 0));
          (* the subband samples, per channel, time slot after time slot *)
          let subbands = if h.layer = 2 then Layer2.decode h bits else Layer3.decode layer3 h s pos in
          let slots = h.samples / 32 in
          Array.iteri
            (fun ch samples ->
              let out = if ch = 0 then left else right in
              for t = 0 to slots - 1 do
                Polyphase.synthesize filters.(ch) (Array.sub samples (t * 32) 32) out (!at + (t * 32))
              done)
            subbands;
          if h.channels = 1 then Array.blit left !at right !at h.samples;
          at := !at + h.samples)
        frames;
      Ok (first, { Signal.left; right })
