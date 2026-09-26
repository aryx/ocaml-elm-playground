(* Hmac: a hash with a key -- a tag that only who knows the key can make
   (Mihir Bellare, Ran Canetti and Hugo Krawczyk, 1996; RFC 2104).

       HMAC(K, m) = H((K xor opad) || H((K xor ipad) || m))

   K padded with zeros to the hash's block (64 bytes for SHA-256, 128
   for SHA-384), or first hashed if longer; ipad the byte 0x36 repeated,
   opad 0x5c. Two hashes, one inside the other, so that knowing H(K ||
   m) -- which a Merkle-Damgard hash lets you extend to H(K || m || more)
   without K, the *length extension* -- tells nothing.

   Worked example (RFC 4231's test case 2, checked by the tests): the key
   "Jefe", "what do ya want for nothing?", HMAC-SHA-256 5bdcc146 bf60754e
   6a042426 089575c7 5a003f08 9d273983 9dec58b9 64ec3843.

   References: RFC 2104 (1997); RFC 4231 (2005), the SHA-2 test vectors. *)

(* [hmac ~hash ~block key message] *)
val hmac : hash:(string -> string) -> block:int -> string -> string -> string

val sha256 : string -> string -> string
val sha384 : string -> string -> string
