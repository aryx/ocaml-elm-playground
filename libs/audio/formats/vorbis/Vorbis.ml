(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Vorbis.mli *)

let fail (what : string) = failwith ("Vorbis: " ^ what)

(*****************************************************************************)
(* Bits: the low ones of each byte first *)
(*****************************************************************************)

(* a packet read past its end: what was being decoded is left as it is *)
exception End_of_packet

type bits = { data : string; mutable at : int (* in bits *) }

let read (b : bits) (n : int) : int =
  let v = ref 0 in
  for i = 0 to n - 1 do
    let byte = b.at lsr 3 in
    if byte >= String.length b.data then raise End_of_packet;
    v := !v lor (((Char.code (String.unsafe_get b.data byte) lsr (b.at land 7)) land 1) lsl i);
    b.at <- b.at + 1
  done;
  !v

let flag (b : bits) : bool = read b 1 = 1

(* how many bits a number takes: 0 for 0, 3 for 4 to 7 *)
let rec ilog (x : int) : int = if x <= 0 then 0 else 1 + ilog (x lsr 1)

(*****************************************************************************)
(* Codebooks *)
(*****************************************************************************)

type codebook = {
  dimensions : int;
  (* the codes as a tree: at 2 * node + bit, the next node, or minus an
   * entry and one; 0 where no code goes *)
  tree : int array;
  (* each entry's vector, [dimensions] numbers: empty if the book only
   * gives numbers *)
  vectors : float array;
}

(* 32 bits as a number: a mantissa of 21 bits, its sign, an exponent *)
let float32_unpack (x : int) : float =
  let mantissa = float_of_int (x land 0x1fffff) and exponent = (x land 0x7fe00000) lsr 21 in
  Float.ldexp (if x land 0x80000000 <> 0 then -.mantissa else mantissa) (exponent - 788)

(* the codes, from their lengths: each entry in turn takes the first
 * free code of its length, the tree filled from the left; -1 for an
 * entry with none *)
let codewords (lengths : int array) : int array =
  (* available.(l): a free code of length l, written from bit 31 down *)
  let available = Array.make 33 0 and first = ref true in
  Array.map
    (fun length ->
      if length = 0 then -1
      else if !first then (
        first := false;
        for l = 1 to length do available.(l) <- 1 lsl (32 - l) done;
        0)
      else (
        let z = ref length in
        while !z > 0 && available.(!z) = 0 do decr z done;
        if !z = 0 then fail "a codebook with more codes than fit";
        let code = available.(!z) in
        available.(!z) <- 0;
        for y = length downto !z + 1 do available.(y) <- code + (1 lsl (32 - y)) done;
        code lsr (32 - length)))
    lengths

let tree_of_lengths (lengths : int array) : int array =
  let tree = Array.make (2 * max 2 (Array.fold_left ( + ) 0 lengths + 1)) 0 and nodes = ref 1 in
  let codes = codewords lengths in
  Array.iteri
    (fun entry length ->
      let node = ref 0 in
      for i = length - 1 downto 0 do
        let slot = (2 * !node) + ((codes.(entry) lsr i) land 1) in
        if i = 0 then tree.(slot) <- -(entry + 1)
        else (
          if tree.(slot) <= 0 then (
            tree.(slot) <- !nodes;
            incr nodes);
          node := tree.(slot))
      done)
    lengths;
  tree

let read_codebook (b : bits) : codebook =
  if read b 24 <> 0x564342 then fail "a codebook that does not start as one";
  let dimensions = read b 16 in
  let entries = read b 24 in
  let lengths = Array.make entries 0 in
  if not (flag b) then (
    let sparse = flag b in
    for i = 0 to entries - 1 do if (not sparse) || flag b then lengths.(i) <- read b 5 + 1 done)
  else (
    (* in order of length: how many entries have each *)
    let entry = ref 0 and length = ref (read b 5 + 1) in
    while !entry < entries do
      let count = read b (ilog (entries - !entry)) in
      if !entry + count > entries then fail "a codebook's lengths past its entries";
      Array.fill lengths !entry count !length;
      entry := !entry + count;
      incr length
    done);
  let vectors =
    match read b 4 with
    | 0 -> [||]
    | (1 | 2) as kind ->
        let minimum = float32_unpack (read b 32) in
        let delta = float32_unpack (read b 32) in
        let value_bits = read b 4 + 1 in
        let sequence = flag b in
        (* kind 1: a lattice, each dimension one of [values] numbers;
         * kind 2: every entry's numbers written out *)
        let values =
          if kind = 2 then entries * dimensions
          else (
            let r = ref (int_of_float (Float.floor (Float.pow (float_of_int entries) (1. /. float_of_int dimensions)))) in
            let power r = let p = ref 1 in for _ = 1 to dimensions do p := !p * r done; !p in
            while power (!r + 1) <= entries do incr r done;
            while !r > 0 && power !r > entries do decr r done;
            !r)
        in
        let multiplicands = Array.init values (fun _ -> float_of_int (read b value_bits)) in
        let v = Array.make (entries * dimensions) 0. in
        for e = 0 to entries - 1 do
          let last = ref 0. and divisor = ref 1 in
          for i = 0 to dimensions - 1 do
            let m = if kind = 2 then multiplicands.((e * dimensions) + i) else multiplicands.(e / !divisor mod values) in
            let x = (m *. delta) +. minimum +. !last in
            if sequence then last := x;
            v.((e * dimensions) + i) <- x;
            if kind = 1 then divisor := !divisor * values
          done
        done;
        v
    | _ -> fail "a codebook's lookup of a kind not known"
  in
  { dimensions; tree = tree_of_lengths lengths; vectors }

(* an entry's number: bits down the tree *)
let scalar (b : bits) (book : codebook) : int =
  let rec go node =
    let next = book.tree.((2 * node) + read b 1) in
    if next > 0 then go next else if next < 0 then -next - 1 else fail "a code that is none"
  in
  go 0

(*****************************************************************************)
(* The setup: floors, residues, mappings, modes *)
(*****************************************************************************)

type floor = {
  partition_classes : int array;
  class_dimensions : int array;
  class_subclasses : int array;
  class_masterbooks : int array;
  subclass_books : int array array; (* -1: none *)
  multiplier : int;
  xs : int array; (* where the curve's points are, as sent *)
  order : int array; (* their indexes, by where they are *)
  low : int array; (* each point's neighbours among those before it: just below, just above *)
  high : int array;
}

type residue = { kind : int; start : int; stop : int; partition_size : int; classifications : int; classbook : int; books : int array array (* -1: none *) }
type mapping = { couplings : (int * int) array; mux : int array; submaps : (int * int) array (* a floor, a residue *) }
type mode = { long : bool; mapping : int }

type t = {
  channels : int;
  rate : int;
  short : int; (* the two sizes of a block *)
  long_ : int;
  codebooks : codebook array;
  floors : floor array;
  residues : residue array;
  mappings : mapping array;
  modes : mode array;
  (* the second half of the block before, windowed: what the next one's first half is added to *)
  mutable previous : float array array option;
}

let read_floor (b : bits) : floor =
  if read b 16 <> 1 then fail "a floor of kind 0 (not decoded: no encoder since 2001 makes one)";
  let partitions = read b 5 in
  let partition_classes = Array.init partitions (fun _ -> read b 4) in
  let classes = 1 + Array.fold_left max (-1) partition_classes in
  let class_dimensions = Array.make classes 0 and class_subclasses = Array.make classes 0 and class_masterbooks = Array.make classes 0 in
  let subclass_books =
    Array.init classes (fun c ->
        class_dimensions.(c) <- read b 3 + 1;
        class_subclasses.(c) <- read b 2;
        if class_subclasses.(c) > 0 then class_masterbooks.(c) <- read b 8;
        Array.init (1 lsl class_subclasses.(c)) (fun _ -> read b 8 - 1))
  in
  let multiplier = read b 2 + 1 in
  let range_bits = read b 4 in
  let xs = ref [ 1 lsl range_bits; 0 ] in
  Array.iter (fun c -> for _ = 1 to class_dimensions.(c) do xs := read b range_bits :: !xs done) partition_classes;
  let xs = Array.of_list (List.rev !xs) in
  let n = Array.length xs in
  let order = Array.init n Fun.id in
  Array.stable_sort (fun i j -> compare xs.(i) xs.(j)) order;
  let neighbour i better =
    let found = ref 0 and any = ref false in
    for j = 0 to i - 1 do if better xs.(j) xs.(i) && ((not !any) || better xs.(!found) xs.(j)) then (found := j; any := true) done;
    !found
  in
  { partition_classes; class_dimensions; class_subclasses; class_masterbooks; subclass_books; multiplier; xs; order;
    low = Array.init n (fun i -> if i < 2 then 0 else neighbour i (fun a b -> a < b));
    high = Array.init n (fun i -> if i < 2 then 0 else neighbour i (fun a b -> a > b)) }

let read_residue (b : bits) : residue =
  let kind = read b 16 in
  if kind > 2 then fail "a residue of a kind not known";
  let start = read b 24 in
  let stop = read b 24 in
  let partition_size = read b 24 + 1 in
  let classifications = read b 6 + 1 in
  let classbook = read b 8 in
  let cascades =
    Array.init classifications (fun _ ->
        let low = read b 3 in
        let high = if flag b then read b 5 else 0 in
        (high * 8) + low)
  in
  let books = Array.map (fun cascade -> Array.init 8 (fun pass -> if cascade land (1 lsl pass) <> 0 then read b 8 else -1)) cascades in
  { kind; start; stop; partition_size; classifications; classbook; books }

let read_mapping (b : bits) (channels : int) : mapping =
  if read b 16 <> 0 then fail "a mapping of a kind not known";
  let submaps = if flag b then read b 4 + 1 else 1 in
  let couplings =
    if flag b then
      Array.init (read b 8 + 1) (fun _ ->
          let magnitude = read b (ilog (channels - 1)) in
          let angle = read b (ilog (channels - 1)) in
          (magnitude, angle))
    else [||]
  in
  if read b 2 <> 0 then fail "a mapping's reserved bits";
  let mux = if submaps > 1 then Array.init channels (fun _ -> read b 4) else Array.make channels 0 in
  let submaps =
    Array.init submaps (fun _ ->
        ignore (read b 8);
        let floor = read b 8 in
        let residue = read b 8 in
        (floor, residue))
  in
  { couplings; mux; submaps }

let create ~(identification : string) ~(setup : string) : t =
  let header (s : string) (kind : int) : bits =
    if String.length s < 7 || Char.code s.[0] <> kind || String.sub s 1 6 <> "vorbis" then fail "a header that is not one";
    { data = s; at = 56 }
  in
  let b = header identification 1 in
  if read b 32 <> 0 then fail "a version not known";
  let channels = read b 8 in
  let rate = read b 32 in
  ignore (read b 32, read b 32, read b 32);
  let short = 1 lsl read b 4 in
  let long_ = 1 lsl read b 4 in
  if channels = 0 || rate = 0 || short > long_ then fail "an identification that cannot be";
  let b = header setup 5 in
  let several : 'a. int -> (unit -> 'a) -> 'a array = fun bits f -> Array.init (read b bits + 1) (fun _ -> f ()) in
  let codebooks = several 8 (fun () -> read_codebook b) in
  (* the transforms in time: placeholders, all zero *)
  ignore (several 6 (fun () -> read b 16));
  let floors = several 6 (fun () -> read_floor b) in
  let residues = several 6 (fun () -> read_residue b) in
  let mappings = several 6 (fun () -> read_mapping b channels) in
  let modes =
    several 6 (fun () ->
        let long = flag b in
        ignore (read b 16, read b 16);
        { long; mapping = read b 8 })
  in
  { channels; rate; short; long_; codebooks; floors; residues; mappings; modes; previous = None }

let channels (t : t) : int = t.channels
let rate (t : t) : int = t.rate

(*****************************************************************************)
(* The transform back: frequencies to samples *)
(*****************************************************************************)

(* a Fourier transform of a power of two of complex numbers, in place *)
let fft (re : float array) (im : float array) : unit =
  let n = Array.length re in
  let j = ref 0 in
  for i = 0 to n - 2 do
    if i < !j then (
      let t = re.(i) in re.(i) <- re.(!j); re.(!j) <- t;
      let t = im.(i) in im.(i) <- im.(!j); im.(!j) <- t);
    let m = ref (n lsr 1) in
    while !m >= 1 && !j land !m <> 0 do j := !j lxor !m; m := !m lsr 1 done;
    j := !j lor !m
  done;
  let len = ref 2 in
  while !len <= n do
    let half = !len / 2 and angle = -2. *. Float.pi /. float_of_int !len in
    let wr = Float.cos angle and wi = Float.sin angle in
    let i = ref 0 in
    while !i < n do
      let cr = ref 1. and ci = ref 0. in
      for k = 0 to half - 1 do
        let a = !i + k and b = !i + k + half in
        let tr = (re.(b) *. !cr) -. (im.(b) *. !ci) and ti = (re.(b) *. !ci) +. (im.(b) *. !cr) in
        re.(b) <- re.(a) -. tr;
        im.(b) <- im.(a) -. ti;
        re.(a) <- re.(a) +. tr;
        im.(a) <- im.(a) +. ti;
        let c = (!cr *. wr) -. (!ci *. wi) in
        ci := (!cr *. wi) +. (!ci *. wr);
        cr := c
      done;
      i := !i + !len
    done;
    len := !len * 2
  done

(* u.(n) = the sum over k of x.(k) cos (pi / m (n + 1/2) (k + 1/2)):
 * the cosine transform the MDCT is made of, by its definition *)
let dct4_simple (x : float array) : float array =
  let m = Array.length x in
  Array.init m (fun n ->
      let sum = ref 0. in
      for k = 0 to m - 1 do sum := !sum +. (x.(k) *. Float.cos (Float.pi /. float_of_int m *. (float_of_int n +. 0.5) *. (float_of_int k +. 0.5))) done;
      !sum)

(* claude: opti: the same by a Fourier transform of half the size: the
 * pairs (x.(2j), x.(m-1-2j)) as complex numbers, turned before (by
 * (4j+1) pi / 4m) and after (by k pi / m): with the transform's own
 * 2 pi jk / (m/2), that is (4j+1)(4k+1) pi / 4m, the cosine's angle. A block of 2048 samples: 1024 x 1024 cosines, or 512 log 512
 * (measured: a second of sound decoded in 1.2 s, then in 0.03) *)
let dct4 (x : float array) : float array =
  let m = Array.length x in
  if m < 4 then dct4_simple x
  else (
    let h = m / 2 in
    let re = Array.make h 0. and im = Array.make h 0. in
    let turn j = -.Float.pi *. float_of_int ((4 * j) + 1) /. float_of_int (4 * m) in
    for j = 0 to h - 1 do
      let a = x.(2 * j) and b = x.(m - 1 - (2 * j)) and c = Float.cos (turn j) and s = Float.sin (turn j) in
      re.(j) <- (a *. c) -. (b *. s);
      im.(j) <- (a *. s) +. (b *. c)
    done;
    fft re im;
    let u = Array.make m 0. in
    for k = 0 to h - 1 do
      let after = -.Float.pi *. float_of_int k /. float_of_int m in
      let c = Float.cos after and s = Float.sin after in
      u.(2 * k) <- (re.(k) *. c) -. (im.(k) *. s);
      u.(m - 1 - (2 * k)) <- -.((re.(k) *. s) +. (im.(k) *. c))
    done;
    u)

(* a block's n/2 frequencies as its n samples: the cosine transform's
 * values, unfolded by its symmetries *)
let imdct (x : float array) : float array =
  let m = Array.length x in
  let u = dct4 x in
  Array.init (2 * m) (fun n -> if n < m / 2 then u.(n + (m / 2)) else if n < 3 * m / 2 then -.u.((3 * m / 2) - 1 - n) else -.u.(n - (3 * m / 2)))

(*****************************************************************************)
(* A packet *)
(*****************************************************************************)

(* a floor's curve is drawn in decibels: a step is 140 / 256 dB *)
let from_db : float array = Array.init 256 (fun i -> Float.pow 10. (float_of_int (i - 255) *. 0.02734375))

(* the line from (x0, y0) to (x1, y1), in whole numbers: where it is at x *)
let point (x0 : int) (y0 : int) (x1 : int) (y1 : int) (x : int) : int =
  let dy = y1 - y0 and adx = x1 - x0 in
  let off = abs dy * (x - x0) / adx in
  if dy < 0 then y0 - off else y0 + off

(* a floor's points read: None when the channel is silent in this block *)
let read_floor_points (t : t) (b : bits) (f : floor) : int array option =
  if not (flag b) then None
  else (
    let range = [| 256; 128; 86; 64 |].(f.multiplier - 1) in
    let ys = Array.make (Array.length f.xs) 0 in
    ys.(0) <- read b (ilog (range - 1));
    ys.(1) <- read b (ilog (range - 1));
    let offset = ref 2 in
    Array.iter
      (fun c ->
        let bits = f.class_subclasses.(c) in
        let value = ref (if bits > 0 then scalar b t.codebooks.(f.class_masterbooks.(c)) else 0) in
        for j = 0 to f.class_dimensions.(c) - 1 do
          let book = f.subclass_books.(c).(!value land ((1 lsl bits) - 1)) in
          value := !value lsr bits;
          ys.(!offset + j) <- (if book >= 0 then scalar b t.codebooks.(book) else 0)
        done;
        offset := !offset + f.class_dimensions.(c))
      f.partition_classes;
    Some ys)

(* the curve of a floor, n numbers to multiply the residue by: each
 * point's height is what was read, as a difference from the line
 * between its two neighbours; then lines between the points *)
let floor_curve (f : floor) (ys : int array) (n : int) : float array =
  let range = [| 256; 128; 86; 64 |].(f.multiplier - 1) in
  let count = Array.length f.xs in
  let final = Array.make count 0 and used = Array.make count true in
  final.(0) <- ys.(0);
  final.(1) <- ys.(1);
  for i = 2 to count - 1 do
    let lo = f.low.(i) and hi = f.high.(i) in
    let predicted = point f.xs.(lo) final.(lo) f.xs.(hi) final.(hi) f.xs.(i) in
    let v = ys.(i) and high_room = range - predicted and low_room = predicted in
    let room = 2 * min high_room low_room in
    if v <> 0 then (
      used.(lo) <- true;
      used.(hi) <- true;
      final.(i) <-
        (if v >= room then if high_room > low_room then v - low_room + predicted else predicted - v + high_room - 1
         else if v land 1 = 1 then predicted - ((v + 1) / 2)
         else predicted + (v / 2)))
    else (
      used.(i) <- false;
      final.(i) <- predicted)
  done;
  let curve = Array.make n 0 in
  let line x0 y0 x1 y1 =
    let dy = y1 - y0 and adx = x1 - x0 in
    let base = dy / adx in
    let sy = if dy < 0 then base - 1 else base + 1 in
    let ady = abs dy - (abs base * adx) in
    let y = ref y0 and err = ref 0 in
    if x0 < n then curve.(x0) <- !y;
    for x = x0 + 1 to min x1 n - 1 do
      err := !err + ady;
      if !err >= adx then (
        err := !err - adx;
        y := !y + sy)
      else y := !y + base;
      curve.(x) <- !y
    done
  in
  let lx = ref 0 and ly = ref (final.(f.order.(0)) * f.multiplier) in
  for k = 1 to count - 1 do
    let i = f.order.(k) in
    if used.(i) then (
      let hy = final.(i) * f.multiplier and hx = f.xs.(i) in
      if hx > !lx then line !lx !ly hx hy;
      lx := hx;
      ly := hy)
  done;
  if !lx < n then line !lx !ly n !ly;
  Array.map (fun y -> from_db.(max 0 (min 255 y))) curve

(* the residue of some channels' vectors, each of [size] numbers: what
 * is left of the spectrum once the floor is taken out, as vectors of
 * codebooks, in up to eight passes, each finer *)
let read_residue_vectors (t : t) (b : bits) (r : residue) (vectors : float array option array) (size : int) : unit =
  (* kind 2: the channels as one vector, their numbers in turn *)
  let targets, size =
    if r.kind = 2 then if Array.for_all (( = ) None) vectors then ([||], 0) else ([| Some (Array.make (size * Array.length vectors) 0.) |], size * Array.length vectors)
    else (vectors, size)
  in
  let start = min r.start size and stop = min r.stop size in
  let classbook = t.codebooks.(r.classbook) in
  let words = classbook.dimensions in
  let partitions = (stop - start) / r.partition_size in
  (if partitions > 0 && Array.length targets > 0 then
     let classes = Array.map (fun _ -> Array.make (partitions + words) 0) targets in
     try
       for pass = 0 to 7 do
         let p = ref 0 in
         while !p < partitions do
           if pass = 0 then
             Array.iteri
               (fun j target ->
                 if target <> None then (
                   let temp = ref (scalar b classbook) in
                   for i = words - 1 downto 0 do
                     classes.(j).(!p + i) <- !temp mod r.classifications;
                     temp := !temp / r.classifications
                   done))
               targets;
           let i = ref 0 in
           while !i < words && !p < partitions do
             Array.iteri
               (fun j target ->
                 match target with
                 | None -> ()
                 | Some v ->
                     let book = r.books.(classes.(j).(!p)).(pass) in
                     if book >= 0 then (
                       let book = t.codebooks.(book) in
                       let at = start + (!p * r.partition_size) and dim = book.dimensions in
                       if r.kind = 0 then (
                         let step = r.partition_size / dim in
                         for k = 0 to step - 1 do
                           let e = scalar b book * dim in
                           for d = 0 to dim - 1 do v.(at + k + (d * step)) <- v.(at + k + (d * step)) +. book.vectors.(e + d) done
                         done)
                       else (
                         let k = ref 0 in
                         while !k < r.partition_size do
                           let e = scalar b book * dim in
                           for d = 0 to dim - 1 do
                             if !k < r.partition_size then v.(at + !k) <- v.(at + !k) +. book.vectors.(e + d);
                             incr k
                           done
                         done)))
               targets;
             incr p;
             incr i
           done
         done
       done
     with End_of_packet -> ());
  if r.kind = 2 then
    match targets with
    | [| Some whole |] ->
        let n = Array.length vectors in
        Array.iteri (fun j v -> match v with Some v -> Array.iteri (fun i _ -> v.(i) <- whole.((i * n) + j)) v | None -> ()) vectors
    | _ -> ()

(* the window of a block of n: its two slopes, each as long as the
 * shorter of the block and its neighbour on that side *)
let window (t : t) ~(n : int) ~(previous_long : bool) ~(next_long : bool) : float array =
  let slope k size = Float.sin (Float.pi /. 2. *. (Float.sin ((float_of_int k +. 0.5) /. float_of_int size *. Float.pi /. 2.) ** 2.)) in
  let long = n = t.long_ && t.long_ <> t.short in
  let left = if long && not previous_long then t.short / 2 else n / 2 and right = if long && not next_long then t.short / 2 else n / 2 in
  let left_start = (n / 4) - (left / 2) and right_start = (3 * n / 4) - (right / 2) in
  Array.init n (fun i ->
      if i < left_start then 0.
      else if i < left_start + left then slope (i - left_start) left
      else if i < right_start then 1.
      else if i < right_start + right then slope (right - 1 - (i - right_start)) right
      else 0.)

let decode (t : t) (packet : string) : float array array =
  let b = { data = packet; at = 0 } in
  let silence = Array.make t.channels [||] in
  match
    if flag b then None
    else (
      let mode = t.modes.(read b (ilog (Array.length t.modes - 1))) in
      let n = if mode.long then t.long_ else t.short in
      let previous_long = (not mode.long) || flag b in
      let next_long = (not mode.long) || flag b in
      Some (mode, n, previous_long, next_long))
  with
  | exception End_of_packet -> silence
  (* not a packet of sound: a header met again *)
  | None -> silence
  | Some (mode, n, previous_long, next_long) ->
      let mapping = t.mappings.(mode.mapping) in
      let half = n / 2 in
      (* each channel's floor: None for a silent one *)
      let floors =
        Array.init t.channels (fun ch ->
            let f = t.floors.(fst mapping.submaps.(mapping.mux.(ch))) in
            try Option.map (fun ys -> (f, ys)) (read_floor_points t b f) with End_of_packet -> None)
      in
      (* a channel coupled with one that sounds is decoded too *)
      let sounds = Array.map (fun f -> f <> None) floors in
      Array.iter (fun (m, a) -> if sounds.(m) || sounds.(a) then (sounds.(m) <- true; sounds.(a) <- true)) mapping.couplings;
      let spectra = Array.init t.channels (fun _ -> Array.make half 0.) in
      Array.iteri
        (fun s (_, residue) ->
          let members = List.filter (fun ch -> mapping.mux.(ch) = s) (List.init t.channels Fun.id) in
          let vectors = Array.of_list (List.map (fun ch -> if sounds.(ch) then Some spectra.(ch) else None) members) in
          read_residue_vectors t b t.residues.(residue) vectors half)
        mapping.submaps;
      (* two channels sent as one and how far the other is from it: back to two *)
      for i = Array.length mapping.couplings - 1 downto 0 do
        let m, a = mapping.couplings.(i) in
        let mv = spectra.(m) and av = spectra.(a) in
        for j = 0 to half - 1 do
          let x = mv.(j) and y = av.(j) in
          if x > 0. then if y > 0. then av.(j) <- x -. y else (av.(j) <- x; mv.(j) <- x +. y)
          else if y > 0. then av.(j) <- x +. y
          else (av.(j) <- x; mv.(j) <- x -. y)
        done
      done;
      let w = window t ~n ~previous_long ~next_long in
      let blocks =
        Array.mapi
          (fun ch spectrum ->
            match floors.(ch) with
            | None -> Array.make n 0.
            | Some (f, ys) ->
                let curve = floor_curve f ys half in
                let samples = imdct (Array.mapi (fun i x -> x *. curve.(i)) spectrum) in
                Array.mapi (fun i x -> x *. w.(i)) samples)
          spectra
      in
      (* what is finished: from the middle of the block before to the
       * middle of this one, the two halves added where they overlap *)
      let out =
        match t.previous with
        | None -> silence
        | Some previous ->
            let pn = 2 * Array.length previous.(0) in
            let count = (pn / 4) + (n / 4) and shift = (pn / 4) - (n / 4) in
            Array.mapi
              (fun ch block ->
                Array.init count (fun i ->
                    (if i < pn / 2 then previous.(ch).(i) else 0.) +. if i - shift >= 0 && i - shift < half then block.(i - shift) else 0.))
              blocks
      in
      t.previous <- Some (Array.map (fun block -> Array.sub block half half) blocks);
      out

(*****************************************************************************)
(* A whole file *)
(*****************************************************************************)

let of_packets (packets : string list) : t * float array array =
  match packets with
  | identification :: _comment :: setup :: sound ->
      let t = create ~identification ~setup in
      let parts = List.map (decode t) sound in
      (t, Array.init t.channels (fun ch -> Array.concat (List.map (fun (p : float array array) -> p.(ch)) parts)))
  | _ -> fail "fewer than its three headers"

let of_ogg (bytes : string) : t * float array array =
  let t, sound = of_packets (Ogg.packets bytes) in
  match Ogg.length bytes with
  | Some n -> (t, Array.map (fun (c : float array) -> if Array.length c > n then Array.sub c 0 n else c) sound)
  | None -> (t, sound)
