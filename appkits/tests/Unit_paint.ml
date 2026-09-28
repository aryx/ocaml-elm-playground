(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/paint: the .mli's worked examples -- the bits of a row,
 * Apple's own PackBits example, Bresenham's line, the rectangles a
 * picture is drawn with -- and the bucket's two promises: it fills the
 * inside and nothing else, and it finishes, even pouring grey. *)

let t = Testo.create

(* a picture from rows of '#' and '.', and back, which is how a person
   reads one *)
let of_rows rows =
  let b = Bitmap.create ~width:(String.length (List.hd rows)) ~height:(List.length rows) in
  List.iteri (fun y row -> String.iteri (fun x c -> Bitmap.set b x y (c = '#')) row) rows;
  b

let to_rows b =
  List.init (Bitmap.height b) (fun y -> String.init (Bitmap.width b) (fun x -> if Bitmap.get b x y then '#' else '.'))

let check_picture name expected b = Alcotest.(check (list string)) name expected (to_rows b)
let hex s = String.concat " " (List.map (fun c -> Printf.sprintf "%02X" (Char.code c)) (List.of_seq (Bytes.to_seq s)))

let bytes_of_hex s =
  Bytes.of_seq (List.to_seq (List.map (fun h -> Char.chr (int_of_string ("0x" ^ h))) (String.split_on_char ' ' s)))

(* --- Bitmap ------------------------------------------------------------ *)

let test_a_row_packed () =
  let b = of_rows [ "#.##....#." ] in
  Alcotest.(check string) "the leftmost dot in the highest bit" "B0 80" (hex (Bitmap.row b 0))

let test_change_leaves_the_original () =
  let b = Bitmap.create ~width:4 ~height:1 in
  let b' = Bitmap.change b (fun b -> Bitmap.set b 1 0 true) in
  check_picture "the new one" [ ".#.." ] b';
  check_picture "and the one in the history, untouched" [ "...." ] b

let test_rectangles_worked_example () =
  let b = of_rows [ "####.."; "####.."; "..##.." ] in
  Alcotest.(check (list (list int))) "a solid area is one rectangle"
    [ [ 0; 0; 4; 2 ]; [ 2; 2; 2; 1 ] ]
    (List.map (fun (x, y, w, h) -> [ x; y; w; h ]) (Bitmap.rectangles b))

(* the rectangles cover the black dots, each exactly once *)
let test_rectangles_cover_the_dots () =
  let rng = Random.State.make [| 11 |] in
  let b = Bitmap.create ~width:37 ~height:23 in
  for _ = 1 to 300 do
    Bitmap.set b (Random.State.int rng 37) (Random.State.int rng 23) true
  done;
  Paint.fill_rect b Pattern.solid (3, 3) (20, 15);
  let seen = Bitmap.create ~width:37 ~height:23 in
  List.iter
    (fun (x, y, w, h) ->
      for j = y to y + h - 1 do
        for i = x to x + w - 1 do
          if Bitmap.get seen i j then Alcotest.failf "dot (%d,%d) covered twice" i j;
          Bitmap.set seen i j true
        done
      done)
    (Bitmap.rectangles b);
  Alcotest.(check (list string)) "exactly the black dots" (to_rows b) (to_rows seen);
  Alcotest.(check bool) "and the solid block drawn in few" true (List.length (Bitmap.rectangles b) < Bitmap.count b / 3)

let test_saved_and_read_back () =
  let b = Bitmap.create ~width:100 ~height:60 in
  Paint.fill_oval b Pattern.grey (10, 10) (70, 50);
  Paint.frame_rect b Pattern.solid (0, 0) (99, 59);
  let s = Bitmap.to_string b in
  Alcotest.(check (list string)) "the same picture" (to_rows b) (to_rows (Bitmap.of_string s));
  Alcotest.(check bool) "in less room than its bits" true (String.length s < 13 * 60)

(* --- Packbits ---------------------------------------------------------- *)

let tn1023 = "AA AA AA 80 00 2A AA AA AA AA 80 00 2A 22 AA AA AA AA AA AA AA AA AA AA"

let test_apples_example () =
  let packed = Packbits.encode (bytes_of_hex tn1023) in
  Alcotest.(check string) "24 bytes to 15, as TN1023 has it" "FE AA 02 80 00 2A FD AA 03 80 00 2A 22 F7 AA"
    (hex packed);
  let back, pos = Packbits.decode packed ~pos:0 ~len:24 in
  Alcotest.(check string) "and back" tn1023 (hex back);
  Alcotest.(check int) "having read all of it" 15 pos

let test_packbits_round_trip () =
  let rng = Random.State.make [| 3 |] in
  for n = 0 to 300 do
    (* few distinct bytes, so that there are runs of every length *)
    let s = Bytes.init n (fun _ -> Char.chr (Random.State.int rng 3 * 0x55)) in
    let back, _ = Packbits.decode (Packbits.encode s) ~pos:0 ~len:n in
    if back <> s then Alcotest.failf "%d bytes: %s came back %s" n (hex s) (hex back)
  done;
  let noise = Bytes.init 1280 (fun i -> Char.chr (i land 0xFF)) in
  Alcotest.(check int) "at worst, one byte in 128 more" 1290 (Bytes.length (Packbits.encode noise))

(* --- Paint ------------------------------------------------------------- *)

let test_bresenham_worked_example () =
  Alcotest.(check (list (pair int int))) "(0,0) to (5,2)"
    [ (0, 0); (1, 0); (2, 1); (3, 1); (4, 2); (5, 2) ]
    (Paint.line_dots (0, 0) (5, 2));
  Alcotest.(check (list (pair int int))) "and backwards, the same dots" (List.rev (Paint.line_dots (0, 0) (5, 2)))
    (Paint.line_dots (5, 2) (0, 0))

let test_corners_are_inclusive () =
  let b = Bitmap.create ~width:8 ~height:5 in
  Paint.frame_rect b Pattern.solid (5, 3) (2, 1);
  check_picture "from (5,3) back to (2,1)" [ "........"; "..####.."; "..#..#.."; "..####.."; "........" ] b

let test_oval_frame_is_the_edge_of_the_fill () =
  let filled = Bitmap.create ~width:30 ~height:20 and framed = Bitmap.create ~width:30 ~height:20 in
  Paint.fill_oval filled Pattern.solid (2, 3) (27, 16);
  Paint.frame_oval framed Pattern.solid (2, 3) (27, 16);
  for y = 0 to 19 do
    for x = 0 to 29 do
      if Bitmap.get framed x y && not (Bitmap.get filled x y) then Alcotest.failf "(%d,%d) framed, not filled" x y
    done
  done;
  (* pour into the frame: exactly the fill comes out *)
  Seed_fill.fill framed Pattern.solid 15 10;
  Alcotest.(check (list string)) "the frame, filled, is the fill" (to_rows filled) (to_rows framed)

(* --- Seed_fill --------------------------------------------------------- *)

let test_fills_the_inside () =
  let b = of_rows [ "........"; ".######."; ".#....#."; ".#....#."; ".######."; "........" ] in
  Seed_fill.fill b Pattern.solid 3 2;
  check_picture "the inside, and not a dot outside"
    [ "........"; ".######."; ".######."; ".######."; ".######."; "........" ]
    b

let test_a_diagonal_holds () =
  let b = of_rows [ "#...."; ".#..."; "..#.."; "...#."; "....#" ] in
  Seed_fill.fill b Pattern.solid 0 4;
  check_picture "only below the diagonal: corners do not leak"
    [ "#...."; "##..."; "###.."; "####."; "#####" ]
    b

(* the bug the mask is there to prevent *)
let test_grey_finishes () =
  let b = Bitmap.create ~width:40 ~height:30 in
  Paint.frame_rect b Pattern.solid (0, 0) (39, 29);
  Seed_fill.fill b Pattern.grey 20 15;
  (* the inside, 38 by 28, half black; plus the frame *)
  Alcotest.(check int) "half the inside, and the frame" ((38 * 28 / 2) + (2 * 40) + (2 * 28)) (Bitmap.count b)

let test_fills_an_area_of_black_too () =
  let b = of_rows [ "###."; "#.#."; "###." ] in
  Seed_fill.fill b Pattern.white 0 0;
  check_picture "the black ring, made white" [ "...."; "...."; "...." ] b

let tests =
  [
    t "a row of dots, packed" test_a_row_packed;
    t "change leaves the original alone" test_change_leaves_the_original;
    t "rectangles: the worked example" test_rectangles_worked_example;
    t "rectangles cover the dots once" test_rectangles_cover_the_dots;
    t "a picture saved and read back" test_saved_and_read_back;
    t "PackBits: Apple's example" test_apples_example;
    t "PackBits: round trips, and the worst case" test_packbits_round_trip;
    t "Bresenham: the worked example" test_bresenham_worked_example;
    t "corners are inclusive, in any order" test_corners_are_inclusive;
    t "an oval's frame is the edge of its fill" test_oval_frame_is_the_edge_of_the_fill;
    t "the bucket fills the inside" test_fills_the_inside;
    t "a diagonal holds the paint" test_a_diagonal_holds;
    t "pouring grey finishes" test_grey_finishes;
    t "the bucket fills black areas too" test_fills_an_area_of_black_too;
  ]
