(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Weights: the file's bytes, what comes back, what is refused *)

let t = Testo.create

(* the .mli's example, byte for byte *)
let small : Weights.t = { notes = [ ("seed", "7") ]; matrices = [ ("w", Matrix.of_lists [ [ 1.; -2. ] ]) ] }
let small_bytes = "weights 1\nnote seed 7\nmatrix w 1 2\nend\n\x00\x00\x80\x3f\x00\x00\x00\xc0"

let test_bytes () =
  Alcotest.(check string) "the file" small_bytes (Weights.to_string small);
  Alcotest.(check int) "39 bytes of header, 8 of numbers" 47 (String.length small_bytes)

let read (s : string) : Weights.t =
  match Weights.of_string s with Ok w -> w | Error why -> Alcotest.fail why

let test_round_trip () =
  (* a network's worth, its notes with spaces in what they say *)
  let net = Net.make ~seed:5 [ 4; 6; 3 ] in
  let w : Weights.t =
    {
      notes = [ ("seed", "5"); ("result", "11 won, 0 lost, of 20 games") ];
      matrices =
        List.concat (List.mapi (fun i (l : Net.layer) -> [ (Printf.sprintf "%d.w" i, l.w); (Printf.sprintf "%d.b" i, l.b) ]) net);
    }
  in
  let back = read (Weights.to_string w) in
  Alcotest.(check (option string)) "a note" (Some "11 won, 0 lost, of 20 games") (Weights.note back "result");
  Alcotest.(check (option string)) "no such note" None (Weights.note back "games");
  Alcotest.(check (list string)) "the names, in order" (List.map fst w.matrices) (List.map fst back.matrices);
  List.iter2
    (fun (name, (m : Matrix.t)) (_, (m' : Matrix.t)) ->
      Alcotest.(check (pair int int)) (name ^ ", its shape") (m.rows, m.cols) (m'.rows, m'.cols);
      (* single precision: seven digits *)
      Array.iteri (fun i x -> Alcotest.(check (float 1e-6)) (Printf.sprintf "%s, number %d" name i) x m'.data.(i)) m.data)
    w.matrices back.matrices;
  (* 0.1 is not a 32-bit number: its nearest comes back, and that one
     then survives unchanged *)
  let tenth : Weights.t = { notes = []; matrices = [ ("x", Matrix.of_lists [ [ 0.1 ] ]) ] } in
  let once = Weights.to_string tenth in
  let got = (Option.get (Weights.matrix (read once) "x")).data.(0) in
  Alcotest.(check bool) "not 0.1 any more" true (got <> 0.1);
  Alcotest.(check (float 1e-15)) "its nearest single" 0.100000001490116 got;
  Alcotest.(check string) "written again, the same bytes" once (Weights.to_string (read once))

let test_refused () =
  let refused name s =
    match Weights.of_string s with
    | Ok _ -> Alcotest.fail (name ^ ": accepted")
    | Error why -> Alcotest.(check bool) (name ^ ": says why") true (why <> "")
  in
  refused "something else" "P6\n2 2\n255\n";
  refused "another version" "weights 2\nend\n";
  refused "no end" "weights 1\nmatrix w 1 2\n";
  refused "cut short" (String.sub small_bytes 0 (String.length small_bytes - 1));
  refused "too long" (small_bytes ^ "\x00");
  refused "a size that is no number" "weights 1\nmatrix w one 2\nend\n";
  refused "a line of something else" "weights 1\nlayer w 1 2\nend\n";
  (* and the smallest file there is *)
  let empty = read "weights 1\nend\n" in
  Alcotest.(check int) "nothing in it" 0 (List.length empty.matrices)

let tests =
  [
    t "Weights, the bytes of the example" test_bytes;
    t "Weights, written and read back" test_round_trip;
    t "Weights, what is not a weights file" test_refused;
  ]
