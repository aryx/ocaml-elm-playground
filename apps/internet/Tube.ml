(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tube.mli *)

(* the clips: their file in Our_media's playlist, a title, what the
 * format is *)
type clip = { file : string; title : string; format : string; about : string }

let clips : clip list =
  [ { file = "ffmpeg_muxed.mpg"; title = "The ball and the square"; format = "MPEG-1 and MP2, in an .mpg";
      about =
        "A Video CD's formats (1993): MPEG-1 video -- its I, P and B pictures, motion vectors and the DCT -- and MPEG-1 audio layer II, interleaved in a system stream, the picture following the sound's clock. Decoded by graphics/videos/mpeg1 and audio/formats/mpeg_audio." };
    { file = "ball_and_square.avi"; title = "The ball and the square, in an AVI"; format = "Motion JPEG and PCM, in an AVI";
      about = "Video for Windows' container (1992): each frame a JPEG of its own, the sound plain samples, RIFF's chunks around them." };
    { file = "ball_and_square.flc"; title = "The ball and the square, animated"; format = "FLC, Autodesk Animator's";
      about = "Autodesk Animator's animation (1989): the first frame whole, then only what changed, line by line -- what a delta frame is." };
    { file = "ball_and_square.y4m"; title = "The ball and the square, raw"; format = "YUV4MPEG2, raw pictures";
      about = "Nothing compressed: each frame's luma and its chroma at a quarter, as a codec takes them in." };
    { file = "bouncing_ball.gif"; title = "A bouncing ball"; format = "an animated GIF";
      about = "The web's first moving pictures (Netscape 2.0, 1995): a GIF's frames, each with its delay, looped." } ]

let clip_url (c : clip) : string = "about:clip/" ^ c.file

let escape (s : string) : string =
  String.concat "" (List.map (fun c -> match c with '<' -> "&lt;" | '>' -> "&gt;" | '&' -> "&amp;" | c -> String.make 1 c) (List.init (String.length s) (String.get s)))

let style =
  {|<style>
  body { margin: 0; background: #f9f9f9; font-family: sans-serif; color: #0f0f0f }
  header { display: flex; align-items: center; padding: 10px 24px; background: white; border-bottom: 1px solid #e5e5e5 }
  header a { color: #0f0f0f; text-decoration: none; font-size: 22px; font-weight: bold }
  header svg { vertical-align: middle }
  header p { margin: 0 0 0 auto; color: #606060; font-size: 13px }
  main { padding: 16px 24px }
  .grid { display: flex; flex-wrap: wrap; gap: 16px }
  .card { width: 290px }
  .card a { color: #0f0f0f; text-decoration: none }
  .card b { display: block; margin-top: 6px }
  .muted { color: #606060; font-size: 13px }
  .watch { display: flex; gap: 24px }
  .player { flex: 1 }
  .player h1 { font-size: 20px; margin: 10px 0 4px }
  .next { width: 250px }
  .next div { margin-bottom: 10px }
  .next a { color: #0f0f0f; text-decoration: none; font-weight: bold }
  audio { width: 400px }
</style>|}

let header = {|<header><a href="about:tube"><svg width="28" height="20" viewBox="0 0 28 20"><rect width="28" height="20" rx="5" fill="#f00"/><path d="M11 5 L19 10 L11 15 Z" fill="white"/></svg> TinyTube</a><p>a video site of our own: nothing fetched, the codecs ours</p></header>|}

let page (title : string) (body : string) : string =
  Printf.sprintf "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>%s</title>%s</head><body>%s<main>%s</main></body></html>" (escape title) style
    header body

let index () : string =
  page "TinyTube"
    ({|<div class="grid">|}
    ^ String.concat ""
        (List.mapi
           (fun i c ->
             Printf.sprintf {|<div class="card"><a href="about:tube-%d"><video src="%s" width="288" height="216"></video><b>%s</b></a><span class="muted">%s</span></div>|}
               (i + 1) (clip_url c) (escape c.title) (escape c.format))
           clips)
    ^ {|</div><h2>A sound</h2><p class="muted">Two chirps, an MP3 (LAME's; decoded by our own reader):</p><audio controls src="about:clip/lame_encoded.mp3"></audio>|})

let watch (i : int) (c : clip) : string =
  page (c.title ^ " - TinyTube")
    (Printf.sprintf
       {|<div class="watch"><div class="player"><video src="%s" width="640" height="480" controls autoplay></video><h1>%s</h1><p class="muted">%s</p><p>%s</p></div><div class="next"><b>Up next</b>%s</div></div>|}
       (clip_url c) (escape c.title) (escape c.format) (escape c.about)
       (String.concat ""
          (List.mapi (fun j d -> (j, d)) clips
          |> List.filter (fun (j, _) -> j <> i)
          |> List.map (fun (j, (d : clip)) ->
                 Printf.sprintf {|<div><a href="about:tube-%d">%s</a><br><span class="muted">%s</span></div>|} (j + 1) (escape d.title) (escape d.format)))))

let html (s : string) = Some (s, "text/html; charset=utf-8")

let about (name : string) : (string * string) option =
  let file = "clip/" in
  match name with
  | "tube" -> html (index ())
  | _ when String.starts_with ~prefix:"tube-" name -> (
      match Option.bind (int_of_string_opt (String.sub name 5 (String.length name - 5))) (fun n -> Option.map (fun c -> (n - 1, c)) (List.nth_opt clips (n - 1))) with
      | Some (i, c) -> html (watch i c)
      | None -> None)
  | _ when String.starts_with ~prefix:file name ->
      let f = String.sub name (String.length file) (String.length name - String.length file) in
      Option.map (fun bytes -> (Lazy.force bytes, "application/octet-stream")) (List.assoc_opt f Our_media.playlist)
  | _ -> None
