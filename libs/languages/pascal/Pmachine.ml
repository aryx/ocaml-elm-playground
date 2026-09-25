(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Pcode

(* See Pmachine.mli *)

type machine = {
  program : Pcode.program;
  store : int array; (* the stack: every frame, and the expressions *)
  mutable pc : int;
  mutable sp : int; (* the first free word *)
  mutable mp : int; (* the current frame *)
  mutable line : string option; (* the input line being read, and where *)
  mutable col : int;
  out : Buffer.t;
}

(* what stops the machine for a while, or for good *)
type stop = Halted | Slice_over | Need_line | Need_random of int | Failed of int * string

exception Runtime of int * string

(* Turbo Pascal's 64 KB of stack, in words of 16 bits *)
let stack_words = 32768

let wrap (n : int) : int = ((n + 32768) land 0xFFFF) - 32768

let push (m : machine) (v : int) : unit =
  if m.sp >= Array.length m.store then raise (Runtime (202, "Stack overflow"));
  m.store.(m.sp) <- v;
  m.sp <- m.sp + 1

let pop (m : machine) : int =
  m.sp <- m.sp - 1;
  m.store.(m.sp)

let top (m : machine) : int = m.store.(m.sp - 1)

(* the frame [d] static links out from the current one *)
let base (m : machine) (d : int) : int =
  let rec go b d = if d = 0 then b else go m.store.(b + 1) (d - 1) in
  go m.mp d

let pad (width : int) (s : string) : string = if String.length s >= width then s else String.make (width - String.length s) ' ' ^ s

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

(* the input line, when there is one left to read from *)
let rest (m : machine) : string option = match m.line with Some l -> Some (String.sub l m.col (String.length l - m.col)) | None -> None

(* read(i): blanks skipped, the ends of lines included, then the
   integer; None when a line must be typed first *)
let read_integer (m : machine) : int option =
  match m.line with
  | None -> None
  | Some l ->
      let n = String.length l in
      while m.col < n && (l.[m.col] = ' ' || l.[m.col] = '\t') do
        m.col <- m.col + 1
      done;
      if m.col >= n then begin
        m.line <- None;
        None
      end
      else begin
        let start = m.col in
        if l.[m.col] = '-' || l.[m.col] = '+' then m.col <- m.col + 1;
        while m.col < n && l.[m.col] >= '0' && l.[m.col] <= '9' do
          m.col <- m.col + 1
        done;
        match int_of_string_opt (String.sub l start (m.col - start)) with
        | Some v -> Some (wrap v)
        | None -> raise (Runtime (106, "Invalid numeric format"))
      end

(*****************************************************************************)
(* Executing *)
(*****************************************************************************)

let compare (m : machine) (f : int -> int -> bool) : unit =
  let b = pop m in
  let a = pop m in
  push m (if f a b then 1 else 0)

let arith (m : machine) (f : int -> int -> int) : unit =
  let b = pop m in
  let a = pop m in
  push m (wrap (f a b))

(* one instruction; what stops the machine, or None to go on *)
let instruction (m : machine) : stop option =
  let i = m.program.code.(m.pc) in
  m.pc <- m.pc + 1;
  let again () = m.pc <- m.pc - 1 in
  match i with
  | Ldc n -> push m n; None
  | Lod (d, o) -> push m m.store.(base m d + o); None
  | Lda (d, o) -> push m (base m d + o); None
  | Str (d, o) -> m.store.(base m d + o) <- pop m; None
  | Ind o -> let a = pop m in push m m.store.(a + o); None
  | Sto -> let v = pop m in let a = pop m in m.store.(a) <- v; None
  | Ldm n -> let a = pop m in for k = 0 to n - 1 do push m m.store.(a + k) done; None
  | Stm n ->
      m.sp <- m.sp - n;
      let words = m.sp in
      let a = pop m in
      Array.blit m.store words m.store a n;
      None
  | Ixa n -> let k = pop m in let a = pop m in push m (a + (k * n)); None
  | Inc n -> push m (wrap (pop m + n)); None
  | Chk (lo, hi) -> let v = top m in if v < lo || v > hi then raise (Runtime (201, "Range check error")) else None
  | Adi -> arith m ( + ); None
  | Sbi -> arith m ( - ); None
  | Mpi -> arith m ( * ); None
  | Dvi | Mod ->
      if top m = 0 then raise (Runtime (200, "Division by zero"));
      arith m (if i = Dvi then ( / ) else ( mod ));
      None
  | Ngi -> push m (wrap (-pop m)); None
  | Abi -> push m (wrap (abs (pop m))); None
  | Sqi -> let v = pop m in push m (wrap (v * v)); None
  | Odd -> push m (pop m land 1); None
  | Equ -> compare m ( = ); None
  | Neq -> compare m ( <> ); None
  | Les -> compare m ( < ); None
  | Leq -> compare m ( <= ); None
  | Grt -> compare m ( > ); None
  | Geq -> compare m ( >= ); None
  | And -> arith m ( land ); None
  | Ior -> arith m ( lor ); None
  | Not -> push m (1 - pop m); None
  | Ujp l -> m.pc <- l; None
  | Fjp l -> if pop m = 0 then m.pc <- l; None
  | Mst d ->
      let static = base m d in
      push m 0;
      push m static;
      push m m.mp;
      push m 0;
      None
  | Cup (n, l) ->
      let frame = m.sp - n - Pcode.mark in
      m.store.(frame + 3) <- m.pc;
      m.mp <- frame;
      m.pc <- l;
      None
  | Ent n ->
      let stop = m.mp + n in
      if stop >= Array.length m.store then raise (Runtime (202, "Stack overflow"));
      Array.fill m.store m.sp (stop - m.sp) 0;
      m.sp <- stop;
      None
  | Retp | Retf ->
      let frame = m.mp in
      m.pc <- m.store.(frame + 3);
      m.mp <- m.store.(frame + 2);
      (* a function's result, in its frame's first word, stays on top *)
      m.sp <- (if i = Retf then frame + 1 else frame);
      None
  | Stp -> Some Halted
  | Csp c -> (
      match c with
      | Wri -> let w = pop m in Buffer.add_string m.out (pad w (string_of_int (pop m))); None
      | Wrc -> let w = pop m in Buffer.add_string m.out (pad w (String.make 1 (Char.chr (pop m land 255)))); None
      | Wrb -> let w = pop m in Buffer.add_string m.out (pad w (if pop m <> 0 then "TRUE" else "FALSE")); None
      | Wrs s -> let w = pop m in Buffer.add_string m.out (pad w s); None
      | Wln -> Buffer.add_char m.out '\n'; None
      | Rdi -> (
          match read_integer m with
          | Some v -> let a = pop m in m.store.(a) <- v; None
          | None -> again (); Some Need_line)
      | Rdc -> (
          match m.line with
          | None -> again (); Some Need_line
          | Some l ->
              let a = pop m in
              if m.col >= String.length l then begin
                (* the end of a line reads as a space *)
                m.store.(a) <- Char.code ' ';
                m.line <- None
              end
              else begin
                m.store.(a) <- Char.code l.[m.col];
                m.col <- m.col + 1
              end;
              None)
      | Rln -> ( match m.line with None -> again (); Some Need_line | Some _ -> m.line <- None; None)
      | Eol -> (
          match rest m with None -> again (); Some Need_line | Some r -> push m (if r = "" then 1 else 0); None)
      | Rnd -> let n = pop m in if n <= 0 then raise (Runtime (201, "Range check error")) else Some (Need_random n))

(* instructions until something stops the machine, [slice] at most *)
let rec slice (m : machine) (k : int) : stop =
  if k = 0 then Slice_over
  else
    match instruction m with
    | None -> slice m (k - 1)
    | Some stop -> stop
    | exception Runtime (code, msg) -> Failed (code, msg)
    | exception Invalid_argument _ -> Failed (204, "Invalid address")

let run (program : Pcode.program) : unit Talk.talk =
  let m = { program; store = Array.make stack_words 0; pc = 0; sp = 0; mp = 0; line = None; col = 0; out = Buffer.create 256 } in
  (* what was written, printed before the machine stops *)
  let flush (k : unit -> unit Talk.talk) : unit Talk.talk =
    let s = Buffer.contents m.out in
    Buffer.clear m.out;
    if s = "" then k () else Talk.Print (s, k ())
  in
  let rec go () =
    match slice m 5000 with
    | Halted -> flush (fun () -> Talk.Done ())
    | Slice_over -> flush (fun () -> Talk.Step go)
    | Need_line -> flush (fun () -> Talk.Read_line (fun l -> m.line <- Some l; m.col <- 0; go ()))
    | Need_random n -> flush (fun () -> Talk.Random (n, fun k -> push m k; go ()))
    | Failed (code, msg) ->
        let line = if m.pc > 0 then program.lines.(m.pc - 1) else 0 in
        flush (fun () -> Talk.Print (Printf.sprintf "\nRuntime error %d at line %d: %s\n" code line msg, Talk.Done ()))
  in
  go ()

let execute (source : string) (answers : string list) : string =
  match Pascal_compile.compile source with
  | Ok p -> Talk.run (run p) answers
  | Error e -> Printf.sprintf "Error at %d:%d: %s\n" e.line e.col e.message
