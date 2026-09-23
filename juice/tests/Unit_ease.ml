(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_ease.mli *)

let close = Alcotest.float 1e-9

(* the highest value of [f] on [0, 1], and where, sampled every 1e-5 *)
let peak (f : Ease.t) : float * float =
  let best = ref (f 0., 0.) in
  for i = 1 to 100_000 do
    let t = float_of_int i /. 100_000. in
    if f t > fst !best then best := (f t, t)
  done;
  !best

let tests =
  Testo.categorize "Ease"
    [
      Testo.create "the worked example: quad at a half" (fun () ->
          Alcotest.check close "in" 0.25 (Ease.quad 0.5);
          Alcotest.check close "out" 0.75 (Ease.out Ease.quad 0.5);
          Alcotest.check close "in_out, at a half: halfway" 0.5 (Ease.in_out Ease.quad 0.5);
          Alcotest.check close "in_out, at a quarter: the in curve, halved" 0.125 (Ease.in_out Ease.quad 0.25));
      Testo.create "the worked example: out back overshoots by 10%" (fun () ->
          let v, t = peak (Ease.out Ease.back) in
          Alcotest.check (Alcotest.float 1e-4) "up to 1.1" 1.1 v;
          Alcotest.check (Alcotest.float 1e-3) "at 0.58" 0.580 t);
      Testo.create "every curve is 0 at 0 and 1 at 1" (fun () ->
          List.iter
            (fun (name, f) ->
              Alcotest.check close (name ^ " at 0") 0. (f 0.);
              Alcotest.check close (name ^ " at 1") 1. (f 1.))
            Ease.all);
      Testo.create "out of out is the curve again" (fun () ->
          List.iter
            (fun (name, f) ->
              List.iter
                (fun t -> Alcotest.check close (Printf.sprintf "%s at %g" name t) (f t) (Ease.out (Ease.out f) t))
                [ 0.1; 0.3; 0.5; 0.7; 0.9 ])
            Ease.all);
      Testo.create "the bounces touch 1 as a ball touches the ground" (fun () ->
          let out_bounce = Ease.out Ease.bounce in
          List.iter
            (fun t -> Alcotest.check (Alcotest.float 1e-6) (Printf.sprintf "at %g" t) 1. (out_bounce t))
            [ 1. /. 2.75; 2. /. 2.75; 2.5 /. 2.75 ]);
      Testo.create "the gentle ones stay in [0, 1] and never go back" (fun () ->
          List.iter
            (fun (name, f) ->
              let prev = ref 0. in
              for i = 1 to 1000 do
                let v = f (float_of_int i /. 1000.) in
                if v < !prev -. 1e-12 || v > 1. +. 1e-12 then Alcotest.failf "%s goes back or beyond at %d/1000" name i;
                prev := v
              done)
            (List.filter
               (fun (name, _) ->
                 not (List.exists (fun w -> String.ends_with ~suffix:w name) [ "back"; "elastic"; "bounce" ]))
               Ease.all));
    ]
