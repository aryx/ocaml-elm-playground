(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Web_audio.mli *)

open Basics

(*****************************************************************************)
(* Sound: Web Audio *)
(*****************************************************************************)

(* claude: the same sound as natively, every sample ours (Audio.pull,
 * audio/Mixer.mli): each frame, the samples the browser's audio clock
 * will need next are copied into an AudioBuffer, scheduled right after
 * the previous one, 100 ms ahead or more ([ahead]), so that they play back to back
 * with no gap (the Web Audio API: an AudioContext, its currentTime, a
 * buffer source started at a given time; the browser resamples our
 * 44,100 a second to its own rate). Browsers start an AudioContext
 * "suspended" until the page gets a click or a key (their autoplay
 * policy): resumed on the first input event; until then the samples are
 * pulled and dropped, so that sounds don't pile up.
 * The other way, the browser's own OscillatorNodes and GainNodes
 * computing the sound (no samples of ours), is left for comparison
 * (plan_audio_teaching.md, phase 4).
 * References: https://www.w3.org/TR/webaudio/ ;
 * https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API *)
let audio_context : Ojs.t option Lazy.t =
  lazy
    (let ctor = Ojs.get_prop_ascii Ojs.global "AudioContext" in
     if Ojs.type_of ctor = "undefined" then None else Some (Ojs.new_obj ctor [||]))

let audio_state (ctx : Ojs.t) : string = Ojs.string_of_js (Ojs.get_prop_ascii ctx "state")

let resume_audio () : unit =
  match Lazy.force audio_context with
  | Some ctx when audio_state ctx = "suspended" -> ignore (Ojs.call ctx "resume" [||])
  | _ -> ()

(* when the next buffer starts, on the AudioContext's clock *)
let next_start = ref 0.

(* claude: how far ahead the sound is scheduled: a jitter buffer, as
 * networking's Interpolation keeps for packets. Each frame tops the
 * schedule up to [ahead]; a frame later than that and the audio clock
 * runs past the end of the sound: a gap, heard as a cut. A page's
 * first frame builds it whole (TinyMinimoog's, measured in headless
 * Chrome: over 200 ms), a heavy view makes every frame long, a garbage
 * collection one of them. So [ahead] grows by 30 ms at each gap, up to
 * 300 ms, and comes back by 12 ms a second without one, to its least,
 * 50 ms, three frames, the native queue's: a game that keeps up loses
 * nothing, a slow page pays in latency (Audio.latency, which shows it)
 * instead of in cuts. (TinyMinimoog's frames, measured the same way:
 * 12 ms, update and sound 2.2, its 800 shapes built 4.2, turned into a
 * virtual DOM 1.1, the page patched 4.2.) *)
let least_ahead = 0.05
let ahead = ref least_ahead

(* after a frame's [ticks] updates *)
let play_audio (ticks : int) : unit =
  match Lazy.force audio_context with
  | Some ctx when audio_state ctx = "running" ->
      let now = Ojs.float_of_js (Ojs.get_prop_ascii ctx "currentTime") in
      (* late: a gap (the page too slow, or the tab hidden), or the
       * start; begin again half the schedule ahead *)
      if !next_start < now then begin
        if !next_start > 0. then ahead := Float.min 0.3 (!ahead +. 0.03);
        next_start := now +. (!ahead /. 2.)
      end
      else ahead := Float.max least_ahead (!ahead -. 0.0002);
      (* this frame's sounds start with the next buffer, [next_start];
       * then the browser's own processing and output (Web Audio's
       * baseLatency and outputLatency, where it has them) *)
      let seconds prop =
        let v = Ojs.get_prop_ascii ctx prop in
        if Ojs.type_of v = "number" then Ojs.float_of_js v else 0.
      in
      Audio.set_latency (!next_start -. now +. seconds "baseLatency" +. seconds "outputLatency");
      let n = int_of_float ((!ahead -. (!next_start -. now)) *. 44100.) in
      if n > 0 then (
        let samples = Audio.pull n in
        (* two channels, left then right *)
        let buffer = Ojs.call ctx "createBuffer" [| Ojs.int_to_js 2; Ojs.int_to_js n; Ojs.int_to_js 44100 |] in
        List.iteri
          (fun channel samples ->
            let data = Ojs.call buffer "getChannelData" [| Ojs.int_to_js channel |] in
            Array.iteri (fun i x -> Ojs.array_set data i (Ojs.float_to_js x)) samples)
          [ samples.Signal.left; samples.right ];
        let source = Ojs.call ctx "createBufferSource" [||] in
        Ojs.set_prop_ascii source "buffer" buffer;
        ignore (Ojs.call source "connect" [| Ojs.get_prop_ascii ctx "destination" |]);
        ignore (Ojs.call source "start" [| Ojs.float_to_js !next_start |]);
        next_start := !next_start +. (float_of_int n /. 44100.))
  | _ -> ignore (Audio.pull (ticks *.. (44100 /.. 60)))
