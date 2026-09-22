(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Space, and stereo through Synth, the Mixer and Wav: the .mli's
 * pan table, the two pan laws' loudness, distance, Doppler's worked
 * example *)

let t = Testo.create
let power (l, r) = (l *. l) +. (r *. r)
let db ratio = 10. *. log10 ratio

(* the .mli's table, and the loudness: the same everywhere with the
 * constant power law; with the linear one, the middle 3 dB below the
 * sides, the hole in the middle *)
let test_pan () =
  List.iter
    (fun (p, l, r) ->
      let (l', r') = Space.pan p in
      Alcotest.(check (float 0.005)) (Printf.sprintf "pan %g: left" p) l l';
      Alcotest.(check (float 0.005)) (Printf.sprintf "pan %g: right" p) r r';
      Alcotest.(check (float 1e-9)) (Printf.sprintf "pan %g: the power, 2" p) 2. (power (l', r')))
    [ (-1., 1.414, 0.); (-0.5, 1.307, 0.541); (0., 1., 1.); (0.5, 0.541, 1.307); (1., 0., 1.414) ];
  Alcotest.(check (float 0.01)) "linear: the middle against a side (dB)" (-3.01)
    (db (power (Space.pan_linear 0.) /. power (Space.pan_linear 1.)))

let test_positions () =
  let listener = Space.vec 0. 0. 0. and right = Space.vec 1. 0. 0. in
  Alcotest.(check (float 1e-9)) "ahead: the middle" 0. (Space.direction ~listener ~right (Space.vec 0. 10. 0.));
  Alcotest.(check (float 1e-9)) "to the right: 1" 1. (Space.direction ~listener ~right (Space.vec 5. 0. 0.));
  Alcotest.(check (float 1e-9)) "45 degrees left: -sin 45" (-.sqrt 0.5) (Space.direction ~listener ~right (Space.vec (-3.) 3. 0.));
  (* the inverse distance law: -6.02 dB each time the distance doubles *)
  Alcotest.(check (float 1e-9)) "closer than the reference: 1" 1. (Space.attenuation ~reference:10. 5.);
  Alcotest.(check (float 0.01)) "twice the reference (dB)" (-6.02) (20. *. log10 (Space.attenuation ~reference:10. 20.));
  Alcotest.(check (float 0.01)) "four times (dB)" (-12.04) (20. *. log10 (Space.attenuation ~reference:10. 40.))

(* the .mli's car, 30 m/s, c = 343: 1.096 coming, 0.920 going, 3.0
 * semitones between *)
let test_doppler () =
  let still = Space.vec 0. 0. 0. and v = Space.vec 30. 0. 0. in
  let doppler source =
    Space.doppler ~speed_of_sound:343. ~listener:still ~listener_velocity:still ~source ~source_velocity:v
  in
  let coming = doppler (Space.vec (-100.) 0. 0.) and going = doppler (Space.vec 100. 0. 0.) in
  Alcotest.(check (float 0.001)) "coming" 1.096 coming;
  Alcotest.(check (float 0.001)) "going" 0.920 going;
  Alcotest.(check (float 0.01)) "the drop, in semitones" 3.04 (12. *. Float.log2 (coming /. going));
  (* passing right by: moving across the line, no shift *)
  Alcotest.(check (float 1e-9)) "abeam" 1. (doppler (Space.vec 0. 10. 0.));
  (* the listener moving away from a still source: lower *)
  Alcotest.(check (float 1e-6)) "the listener going away" (313. /. 343.)
    (Space.doppler ~speed_of_sound:343. ~listener:(Space.vec 100. 0. 0.) ~listener_velocity:v ~source:still ~source_velocity:still)

(* stereo through the rest: a panned voice's two channels, a sound not
 * panned the same array twice, the mixer's pans gliding, a stereo WAV *)
let test_stereo () =
  let beep = Synth.voice (Wave Sine) 440. |> Synth.lasting 0.1 in
  let plain = Synth.render_stereo beep in
  Alcotest.(check bool) "not panned: one array, in both" true (plain.left == plain.right);
  let right = Synth.render_stereo (Panned (0.5, beep)) and mono = Synth.render beep in
  let (l, r) = Space.pan 0.5 in
  Alcotest.(check (float 1e-9)) "panned right: the left channel" (l *. mono.(1000)) right.left.(1000);
  Alcotest.(check (float 1e-9)) "and the right" (r *. mono.(1000)) right.right.(1000);
  (* a continuous voice moved from the left to the right between two
   * pulls: the pull after glides from one to the other *)
  let m = Mixer.create () in
  let v = { Synth.source = Wave Sine; frequency = 220.; slide = None; seconds = 0.; volume = 0.5; fade = false; effects = []; envelope = None } in
  Mixer.keep ~pan:(-1.) m "car" v;
  ignore (Mixer.pull m 735);
  Mixer.keep ~pan:1. m "car" v;
  let s = Mixer.pull m 735 in
  let level (x : Signal.t) lo hi = Array.fold_left Float.max 0. (Array.map Float.abs (Array.sub x lo (hi - lo))) in
  Alcotest.(check bool) "its start still on the left" true (level s.left 0 50 > 5. *. level s.right 0 50);
  Alcotest.(check bool) "its end on the right" true (level s.right 685 735 > 5. *. level s.left 685 735);
  Mixer.stereo := false;
  Mixer.keep ~pan:1. m "car" v;
  let s = Mixer.pull m 735 in
  Mixer.stereo := true;
  Alcotest.(check bool) "stereo off: the same in both" true (s.left = s.right);
  (* a stereo WAV: 2 channels, 4 bytes a frame, the samples interleaved *)
  let file = Filename.temp_file "stereo" ".wav" in
  Wav.write_stereo file { left = [| 0.5; 0. |]; right = [| -0.5; 1. |] };
  let bytes = In_channel.with_open_bin file In_channel.input_all in
  Sys.remove file;
  Alcotest.(check int) "channels" 2 (String.get_uint16_le bytes 22);
  Alcotest.(check int) "bytes a frame" 4 (String.get_uint16_le bytes 32);
  Alcotest.(check (list int)) "interleaved: L0 R0 L1 R1" [ 16384; -16384; 0; 32767 ]
    (List.init 4 (fun i -> String.get_int16_le bytes (44 + (2 * i))))

let tests =
  Testo.categorize "Space"
    [
      t "the pan laws: constant power, and the hole in the middle" test_pan;
      t "directions and distances" test_positions;
      t "Doppler: the passing car" test_doppler;
      t "stereo: Synth, the Mixer's gliding pans, Wav" test_stereo;
    ]
