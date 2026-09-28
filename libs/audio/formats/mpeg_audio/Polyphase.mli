(* The polyphase filterbank: 32 subbands back into one sound -- the end
   of every MPEG audio decoder, all three layers.

   An MPEG audio encoder first splits the sound into 32 **subbands**, 32
   bands of frequencies of equal width (689 Hz each at 44,100 Hz), each
   *decimated*: a band 1/32 of the spectrum wide needs only 1/32 of the
   samples (Nyquist, Signal.mli), so 32 samples of sound become one
   sample in each of the 32 bands -- as many numbers as before. The
   point: the ear doesn't hear all bands alike (it is deaf to a quiet
   band next to a loud one: *masking* is its name), so the encoder spends its bits
   band by band, many where they are heard, none where they are not.
   Layers I and II code the subband samples directly (Layer2.mli);
   Layer III goes further, splitting each subband again (Layer3.mli).

   The decoder undoes the split: each band's samples, 32 at a time (one
   per band, a *time slot* of them), give back 32 samples of sound. Each band is
   a cosine modulating one lowpass filter, the *prototype*, 512 taps
   long (so the filterbank has memory: the 16 last time slots), the
   standard's steps (ISO/IEC 11172-3, 2.4.3.2, figure A.2):

     S[0..31], the time slot            V: the last 16 time slots,
        |                                  64 values each (1024)
        v
     V[0..63] = sum over k of cos((16 + i)(2k + 1) pi / 64) S[k]
        |          (the matrixing: 64 x 32 multiplications, a DCT)
        v
     U[0..511]: 8 times 32 of V[i * 128 + j], 32 of V[i * 128 + 96 + j]
        |          (the 16 time slots, halves of each chosen)
        v
     out[j] = sum over i of U[j + 32 i] D[j + 32 i], i from 0 to 15
                   (the window: the prototype, signs alternating)

   D, the window (Table B.3), is the prototype filter h, 512 taps
   symmetric about the middle, D[i] = h[i] with the sign flipped in
   every other block of 64: the cosines of the matrixing make those
   signs the right ones for each band. h isn't a formula, but a filter
   the standard's authors designed (its passband 1/64 of the spectrum,
   its first zeros 64 taps apart, like a sinc's):

        1.14 |                   .
             |                  . .
             |                 .   .
             |                .     .
           0 +..........._./         \._...........    512 taps
                 ~0.15 below zero, ~64 taps each side of the peak

   Its 257 values are given here as the standard prints them, which
   happens to be in steps of 2^-16 (the integers of [prototype]).

   The cost, written as the standard does: 64 x 32 multiplications for
   the matrixing and 512 for the window per 32 samples, 80 a sample.
   Real decoders make the matrixing a fast DCT (Byeong Gi Lee, 1984:
   80 multiplications instead of 2048) -- an exercise.

   References: ISO/IEC 11172-3 (1993), 2.4.3.2 and Table B.3; Peter
   Noll, "MPEG Digital Audio Coding", IEEE Signal Processing Magazine
   14 (1997); Davis Pan, "A Tutorial on MPEG/Audio Compression", IEEE
   Multimedia 2 (1995). *)

(* one channel's filterbank: its last 16 time slots *)
type t

val create : unit -> t

(* [synthesize f slot out at]: the 32 subband samples of [slot] (a
 * time slot, the lowest band first) into the 32 samples of sound
 * [out.(at)] to [out.(at + 31)] *)
val synthesize : t -> float array -> float array -> int -> unit

(* h, the prototype's first half and middle, times 2^16: 0, -1, -1, ...,
 * 75,038 (h.(256), 1.144989) *)
val prototype : int array

(* D, the window: 512 coefficients *)
val window : float array
