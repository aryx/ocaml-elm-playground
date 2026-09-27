(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_image.mli *)

module M = St_memory

let magic = "TinySmalltalk80 image 1\n"

(*****************************************************************************)
(* Numbers *)
(*****************************************************************************)

let put_varint (b : Buffer.t) (n : int) : unit =
  let rec go n =
    if n < 128 then Buffer.add_char b (Char.chr n)
    else begin
      Buffer.add_char b (Char.chr (128 lor (n land 127)));
      go (n lsr 7)
    end
  in
  go n

(* an oop: its tag in the first byte's low bit, then its payload -- a
 * SmallInteger's value zigzagged (0 -1 1 -2 2 as 0 1 2 3 4, so that
 * small negatives stay small), or an object's index. Not the oop
 * itself zigzagged: doubling a 32-bit oop overflows on the web, where
 * an int has 32 bits, and each payload here fits in 31. *)
let put_oop (b : Buffer.t) (o : int) : unit =
  let tag, p =
    if M.is_int o then
      let v = M.int_of o in
      (1, if v >= 0 then 2 * v else (-2 * v) - 1)
    else (0, o lsr 1)
  in
  let first = tag lor ((p land 63) lsl 1) in
  if p lsr 6 = 0 then Buffer.add_char b (Char.chr first)
  else begin
    Buffer.add_char b (Char.chr (first lor 128));
    put_varint b (p lsr 6)
  end

type reader = { s : string; mutable pos : int }

let get_varint (r : reader) : int =
  let rec go shift acc =
    if r.pos >= String.length r.s then failwith "truncated image";
    let c = Char.code r.s.[r.pos] in
    r.pos <- r.pos + 1;
    let acc = acc lor ((c land 127) lsl shift) in
    if c < 128 then acc else go (shift + 7) acc
  in
  go 0 0

let get_oop (r : reader) : int =
  if r.pos >= String.length r.s then failwith "truncated image";
  let first = Char.code r.s.[r.pos] in
  r.pos <- r.pos + 1;
  let p = (first lsr 1) land 63 in
  let p = if first >= 128 then p lor (get_varint r lsl 6) else p in
  if first land 1 = 1 then M.of_int (if p land 1 = 0 then p / 2 else -((p + 1) / 2)) else p lsl 1

let get_bytes (r : reader) (n : int) : Bytes.t =
  if r.pos + n > String.length r.s then failwith "truncated image";
  let b = Bytes.of_string (String.sub r.s r.pos n) in
  r.pos <- r.pos + n;
  b

(*****************************************************************************)
(* The known objects *)
(*****************************************************************************)

let known_oops (k : M.known) : int list =
  [
    k.small_integer; k.string; k.symbol; k.array; k.float; k.character; k.compiled_method; k.method_context;
    k.block_context; k.message; k.association; k.point; k.large_positive; k.large_negative; k.metaclass;
    k.method_dictionary; k.true_; k.false_; k.smalltalk;
  ]

let known_of (l : int list) (characters : int array) (specials : int array) : M.known =
  match l with
  | [
   small_integer; string; symbol; array; float; character; compiled_method; method_context; block_context; message;
   association; point; large_positive; large_negative; metaclass; method_dictionary; true_; false_; smalltalk;
  ] ->
      {
        M.small_integer;
        string;
        symbol;
        array;
        float;
        character;
        compiled_method;
        method_context;
        block_context;
        message;
        association;
        point;
        large_positive;
        large_negative;
        metaclass;
        method_dictionary;
        true_;
        false_;
        smalltalk;
        characters;
        special_selectors = specials;
      }
  | _ -> failwith "bad image header"

(*****************************************************************************)
(* Saving and loading *)
(*****************************************************************************)

let save (m : M.t) : string =
  let b = Buffer.create (1 lsl 20) in
  Buffer.add_string b magic;
  let k = M.known m in
  let oops l =
    put_varint b (List.length l);
    List.iter (put_oop b) l
  in
  oops (known_oops k);
  oops (Array.to_list k.characters);
  oops (Array.to_list k.special_selectors);
  let es = M.entries m in
  put_varint b (List.length es);
  List.iter
    (fun (i, cls, body) ->
      put_varint b i;
      put_oop b cls;
      match body with
      | M.Pointers a ->
          Buffer.add_char b 'P';
          oops (Array.to_list a)
      | M.Bytes s ->
          Buffer.add_char b 'B';
          put_varint b (Bytes.length s);
          Buffer.add_bytes b s
      | M.Float f ->
          Buffer.add_char b 'F';
          let bits = Int64.bits_of_float f in
          for j = 0 to 7 do
            Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (8 * j)) 255L)))
          done
      | M.Method (a, s) ->
          Buffer.add_char b 'M';
          oops (Array.to_list a);
          put_varint b (Bytes.length s);
          Buffer.add_bytes b s
      | M.Free -> Buffer.add_char b '-')
    es;
  Buffer.contents b

let load (s : string) : M.t =
  let n = String.length magic in
  if String.length s < n || String.sub s 0 n <> magic then failwith "not a TinySmalltalk80 image";
  let r = { s; pos = n } in
  let oops () = List.init (get_varint r) (fun _ -> get_oop r) in
  let known = oops () in
  let characters = Array.of_list (oops ()) in
  let specials = Array.of_list (oops ()) in
  let k = known_of known characters specials in
  let count = get_varint r in
  let es =
    List.init count (fun _ ->
        let i = get_varint r in
        let cls = get_oop r in
        let tag = r.s.[r.pos] in
        r.pos <- r.pos + 1;
        let body =
          match tag with
          | 'P' -> M.Pointers (Array.of_list (oops ()))
          | 'B' -> M.Bytes (get_bytes r (get_varint r))
          | 'F' ->
              let bytes = get_bytes r 8 in
              let bits = ref 0L in
              for j = 7 downto 0 do
                bits := Int64.logor (Int64.shift_left !bits 8) (Int64.of_int (Char.code (Bytes.get bytes j)))
              done;
              M.Float (Int64.float_of_bits !bits)
          | 'M' ->
              let a = Array.of_list (oops ()) in
              M.Method (a, get_bytes r (get_varint r))
          | _ -> failwith "bad object in image"
        in
        (i, cls, body))
  in
  M.restore k es

let load_vm ?(host = St_boot.quiet_host) (s : string) : St_interp.vm =
  let vm = St_interp.create (load s) host in
  St_primitives.install vm;
  vm
