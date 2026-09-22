(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* ai/Sense and ai/Bot: what a bot may know, and the knobs that make it
 * fair -- the .mli's examples, frame by frame *)

let t = Testo.create

(* Sense.mli's example: a target at 300, sight 600, hearing 200, hidden
 * then out then hidden again *)
let test_sense () =
  let step ~clear target = Sense.update ~sight:600. ~hearing:200. ~distance:300. ~clear ~position:(10., 20.) target in
  let hidden = step ~clear:false Sense.unknown in
  Alcotest.(check bool) "hidden: not visible" false hidden.visible;
  Alcotest.(check bool) "too far to hear" false hidden.audible;
  Alcotest.(check bool) "nothing known" true (Sense.lost hidden);
  let out = step ~clear:true hidden in
  Alcotest.(check bool) "it steps out: visible" true out.visible;
  Alcotest.(check (option (pair (float 1e-9) (float 1e-9)))) "where it is" (Some (10., 20.)) out.position;
  Alcotest.(check int) "age 0" 0 out.age;
  Alcotest.(check int) "seen for a frame" 1 out.seen_for;
  let out = step ~clear:true out in
  Alcotest.(check int) "and another" 2 out.seen_for;
  let gone = step ~clear:false out in
  Alcotest.(check bool) "it hides: not visible" false gone.visible;
  Alcotest.(check (option (pair (float 1e-9) (float 1e-9)))) "the position remembered" (Some (10., 20.)) gone.position;
  Alcotest.(check int) "aging" 1 gone.age;
  Alcotest.(check int) "no longer seen" 0 gone.seen_for;
  (* it gives up after a while *)
  let rec age n t = if n = 0 then t else age (n - 1) (step ~clear:false t) in
  let old = age 60 gone in
  Alcotest.(check int) "61 frames since" 61 old.age;
  Alcotest.(check bool) "still remembered at 120" false (Sense.lost (Sense.forget ~after:120 old));
  Alcotest.(check bool) "forgotten at 30" true (Sense.lost (Sense.forget ~after:30 old));
  (* within hearing: audible through the wall, still not visible *)
  let near = Sense.update ~sight:600. ~hearing:200. ~distance:150. ~clear:false ~position:(0., 0.) Sense.unknown in
  Alcotest.(check bool) "close by: heard" true near.audible;
  Alcotest.(check bool) "but not seen" false near.visible;
  (* the nearest visible one of three *)
  let seen p = Sense.update ~distance:0. ~clear:true ~position:p Sense.unknown in
  Alcotest.(check (option (pair (float 1e-9) (float 1e-9)))) "the nearest visible"
    (Some (2., 0.))
    (Option.bind (Sense.nearest [ (300., seen (3., 0.)); (100., seen (2., 0.)); (50., hidden) ]) (fun t -> t.position))

(* Bot.mli's example: the target steps out at frame 100; with a delay
 * of 15 frames and a rate of 6, the bot shoots at frame 120 *)
let test_delay_and_rate () =
  let world frame = frame >= 100 in
  (* the senses: a bool, "a target is there"; the intent: shoot *)
  let bot = Bot.make ~delay:15 ~rate:6 ~sense:(fun _ there -> there) ~decide:(fun there -> there) () in
  let shot_at bot =
    let r = ref (Bot.start false) and first = ref (-1) in
    for frame = 0 to 200 do
      let (intent, r') = Bot.step bot (world frame) !r in
      r := r';
      if intent && !first < 0 then first := frame
    done;
    !first
  in
  Alcotest.(check int) "delay 15, rate 6: frame 120" 120 (shot_at bot);
  Alcotest.(check int) "delay 15, rate 1: frame 115" 115 (shot_at { bot with rate = 1 });
  (* no knobs at all: the machine, the frame the target appears *)
  Alcotest.(check int) "no delay, every frame: frame 100" 100
    (shot_at (Bot.make ~sense:(fun _ there -> there) ~decide:(fun there -> there) ()));
  (* between two decisions it repeats itself: the target vanishes at
   * 100 again (delay 0, rate 10), and the bot keeps shooting until its
   * next decision *)
  let sticky = Bot.make ~rate:10 ~sense:(fun _ there -> there) ~decide:(fun there -> there) () in
  let r = ref (Bot.start false) and intents = ref [] in
  for frame = 0 to 25 do
    let (intent, r') = Bot.step sticky (frame >= 5 && frame < 12) !r in
    r := r';
    intents := intent :: !intents
  done;
  let intents = List.rev !intents in
  Alcotest.(check bool) "at frame 10: it has seen it" true (List.nth intents 10);
  Alcotest.(check bool) "at 15: gone, but it is still pressing" true (List.nth intents 15);
  Alcotest.(check bool) "at 20: it notices" false (List.nth intents 20);
  (* what it sensed last frame is part of what it senses now: a bot
   * that counts the frames it has seen something keeps the count *)
  let counting = Bot.make ~sense:(fun was there -> if there then (match was with Some n -> n + 1 | None -> 1) else 0)
      ~decide:Fun.id () in
  let r = ref (Bot.start 0) and last = ref 0 in
  for frame = 0 to 9 do
    let (n, r') = Bot.step counting (frame >= 4) !r in
    r := r';
    last := n
  done;
  Alcotest.(check int) "seen for six frames" 6 !last

(* the aim settles: the error halves every [settle] frames a target
 * stays visible, and is the same every run *)
let test_aim () =
  let error seen_for = Bot.aim_error ~spread:20. ~settle:30. ~seen_for ~seed:1 () in
  let worst n = List.fold_left (fun m k -> Float.max m (Float.abs (error k))) 0. (List.init 30 (fun i -> n + i)) in
  Alcotest.(check bool) "at the start, up to the spread" true (worst 0 > 8. && worst 0 <= 20.);
  Alcotest.(check bool) "30 frames on, half of it" true (worst 30 <= 0.5 *. worst 0);
  Alcotest.(check bool) "150 frames on, nearly none" true (worst 150 < 0.05 *. worst 0);
  Alcotest.(check (float 1e-12)) "the same every run" (error 7) (Bot.aim_error ~spread:20. ~settle:30. ~seen_for:7 ~seed:1 ());
  Alcotest.(check bool) "another bot, another wobble" true (Float.abs (error 7 -. Bot.aim_error ~spread:20. ~settle:30. ~seen_for:7 ~seed:2 ()) > 1e-6);
  (* no aim error at all: the machine *)
  Alcotest.(check (float 1e-12)) "spread 0" 0. (Bot.aim_error ~spread:0. ~settle:30. ~seen_for:3 ~seed:1 ())

let tests =
  Testo.categorize "Sense and Bot"
    [
      t "Sense: seen, remembered, forgotten" test_sense;
      t "Bot: the reaction delay and the input rate" test_delay_and_rate;
      t "Bot: an aim that settles" test_aim;
    ]
