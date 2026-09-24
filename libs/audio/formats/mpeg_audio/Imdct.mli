(* The MDCT: Layer III's second split, each subband cut into 18 finer
   frequencies -- and put back together by the decoder.

   Polyphase.mli's 32 subbands are 689 Hz wide at 44,100: too coarse for
   the ear, which tells 100 Hz from 200 Hz. So Layer III splits each
   subband again, into 18 lines, 576 in all (38 Hz each), with the
   *modified discrete cosine transform* (John Princen, Alan Bradley,
   1986-1987; the transform of AAC, Vorbis and Opus since): n samples in,
   n/2 coefficients out --

     X[k] = sum over i of x[i] cos(pi / 2n (2i + 1 + n/2)(2k + 1))

   -- half as many numbers as samples, so it can't be inverted block by
   block. The trick, Princen and Bradley's *time-domain aliasing
   cancellation*: blocks overlap by half, each sample in two, and the
   inverse,

     x'[i] = sum over k of X[k] cos(pi / 2n (2i + 1 + n/2)(2k + 1))

   gives each block back with an error, an *alias* (its halves mirrored
   into each other), whose sign is opposite in the two blocks a sample
   belongs to: added, the aliases cancel, if the window w each block is
   weighed with, once before the MDCT and once after the inverse,
   satisfies w[i]^2 + w[i + n/2]^2 = 1 (a sine does: sin^2 + cos^2):

       block t    |----- w -----|                 n = 36: 18 new
       block t+1         |----- w -----|          samples a block, and
                  ...... : overlap-added :        18 from the block
                                                  before

   The price of fine frequencies is coarse time: a block is 36 samples
   of each subband, 1152 of sound, 26 ms at 44,100 -- and the noise of
   quantization spreads over all of it, heard *before* a sudden attack
   (a castanet's click): the pre-echo. So an encoder switches, for an
   attack, to **short blocks**: 3 MDCTs of 12 (n = 12, 6 lines each,
   the time 3 times finer), with a *start* window (type 1) before and a
   *stop* window (type 3) after, which keep w[i]^2 + w[i + n/2]^2 = 1
   across the change:

     type 0, long    /‾‾‾‾‾‾‾‾\         36 samples, a sine
     type 1, start   /‾‾‾‾‾‾‾‾‾‾|\_     a long rise, a short fall
     type 2, short   3 x /‾‾\           each 12, 6 apart, from 6 to 30
     type 3, stop     _/|‾‾‾‾‾‾‾‾‾\     a short rise, a long fall

   References: J. P. Princen, A. W. Johnson and A. B. Bradley, "Subband/
   Transform Coding Using Filter Bank Designs Based on Time Domain
   Aliasing Cancellation", ICASSP 1987; ISO/IEC 11172-3 (1993),
   2.4.3.4.10. *)

(* [imdct coefficients]: the n/2 coefficients' n samples, n = 36 or 12
 * (as the standard writes it, not scaled) *)
val imdct : float array -> float array

(* [mdct samples]: the forward transform, the encoder's: n samples, n/2
 * coefficients; imdct (mdct x), each block windowed twice and the
 * blocks overlapped by half, is x times n/4 *)
val mdct : float array -> float array

(* [window block_type]: the 36 weights of a long block's window, type 0,
 * 1 or 3 (the start and stop windows around short blocks); 2, a short
 * block's 12 *)
val window : int -> float array
