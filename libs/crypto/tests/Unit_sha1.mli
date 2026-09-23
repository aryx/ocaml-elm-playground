(* crypto/Sha1: FIPS 180's test vectors, the .mli's worked examples,
 * and the lengths around a block's edge (55, 56, 64 bytes: the padding
 * spilling into a second block) *)
val tests : Testo.t list
