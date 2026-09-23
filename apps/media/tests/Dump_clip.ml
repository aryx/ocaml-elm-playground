(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* our clip (Our_media.clip, drawn by graphics/2d), 25 frames a second,
 * as Y4M on stdout: what make_clips.sh gives ffmpeg to encode *)
let () =
  set_binary_mode_out stdout true;
  print_string (Y4m.to_string ~rate:(25, 1) (Lazy.force Our_media.clip))
