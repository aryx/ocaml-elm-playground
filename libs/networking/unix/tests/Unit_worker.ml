(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_worker.mli *)

(* the jobs polled like a frame loop would, until all are done: their
 * results, and how long it took *)
let finish (jobs : 'a Worker.job list) : ('a, exn) result list * float =
  let t0 = Unix.gettimeofday () in
  let rec go () =
    match List.map Worker.poll jobs with
    | results when List.for_all Option.is_some results -> List.map Option.get results
    | _ ->
        Unix.sleepf (1. /. 60.);
        go ()
  in
  let results = go () in
  (results, Unix.gettimeofday () -. t0)

let tests =
  Testo.categorize "Worker"
    [
      Testo.create "four waits at the same time, not one after the other" (fun () ->
          let pool = Worker.create 4 in
          let t0 = Unix.gettimeofday () in
          let jobs = List.init 4 (fun i -> Worker.submit pool (fun () -> Unix.sleepf 0.2; i)) in
          let submitted = Unix.gettimeofday () -. t0 in
          let results, took = finish jobs in
          Alcotest.(check bool) (Printf.sprintf "submitted in %.1f ms: nothing waited" (submitted *. 1000.)) true
            (submitted < 0.01);
          Alcotest.(check (list int)) "each its result" [ 0; 1; 2; 3 ] (List.map Result.get_ok results);
          (* claude: 0.2 s together; one after the other would be 0.8 *)
          Alcotest.(check bool) (Printf.sprintf "all done in %.2f s" took) true (took < 0.5));
      Testo.create "more jobs than threads: queued" (fun () ->
          let pool = Worker.create 2 in
          let results, took = finish (List.init 4 (fun i -> Worker.submit pool (fun () -> Unix.sleepf 0.1; i))) in
          Alcotest.(check (list int)) "each its result" [ 0; 1; 2; 3 ] (List.map Result.get_ok results);
          Alcotest.(check bool) (Printf.sprintf "two rounds, %.2f s" took) true (took >= 0.2));
      Testo.create "an exception is a result" (fun () ->
          let pool = Worker.create 1 in
          match finish [ Worker.submit pool (fun () -> failwith "no") ] with
          | [ Error (Failure msg) ], _ when msg = "no" -> ()
          | _ -> Alcotest.fail "the exception lost");
    ]
