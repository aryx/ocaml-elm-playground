(* Checksum: is your model the same as mine? A number that says so.

   Lockstep (plan_networking_teaching.md) sends inputs, never the game:
   each machine runs the same update on the same inputs and trusts that
   they compute the same model. If one doesn't -- a clock read in
   update, an unseeded Random, a hash table iterated in another order
   (notes_networking.md section 8) -- the two games drift apart
   silently: a *desync*, the bug that sounds like a ghost story ("his
   tank was over there on my screen"). So every second or so, each
   machine sends a small fingerprint of its model, and the first
   mismatch says when the drift began: the difference between a bug and
   a ghost story.

   The fingerprint: the model's bytes (Marshal, OCaml's own
   serialization, without sharing, so that two equal models give the
   same bytes however they were built), then hashed with FNV-1a:

       hash = 2166136261                  (the offset basis)
       for each byte b:
         hash = (hash xor b) * 16777619   (the FNV prime), mod 2^32

   The xor mixes the byte in, the multiplication by a prime spreads it
   over all 32 bits. Not cryptographic: it catches accidents, not a
   cheater, who could forge a matching model (that is the server's
   job, notes_networking.md). Computed on Int32, which wraps at 2^32 in
   native code and in a browser alike.

   Worked examples (checked by the tests, the published FNV-1a test
   vectors): "" gives 0x811c9dc5 (the offset basis, nothing mixed in),
   "a" 0xe40c292c, "foobar" 0xbf9cf968.

   Two limits, both said where they bite: a model holding functions
   can't be marshalled (a closure isn't data), so [of_model] raises;
   and Marshal's bytes, and so the checksums, differ between native
   code and js_of_ocaml (63 and 32-bit ints), which matters only when a
   browser plays a native player (the plan's phase 5 serializes with
   its own Wire instead).

   References: Glenn Fowler, Landon Curt Noll and Kiem-Phong Vo, the
   FNV hash (1991), its FNV-1a variant, and the test vectors in the
   IETF draft "The FNV Non-Cryptographic Hash Algorithm"; Mark Terrano
   and Paul Bettner, "1500 Archers on a 28.8" (GDC 2001), for the
   out-of-sync checks of Age of Empires. *)

(* FNV-1a, 32 bits, of some bytes *)
val fnv1a : string -> int32

(* the checksum of any value without functions in it (Marshal, without
 * sharing, then fnv1a); raises Invalid_argument on a functional value *)
val of_model : 'a -> int32

(* "8a3f0c1d": how a checksum is shown, in a log or on the screen *)
val to_hex : int32 -> string
