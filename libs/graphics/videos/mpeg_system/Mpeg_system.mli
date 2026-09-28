(* MPEG-1 system streams (.mpg): a video and its sound, interleaved.

   MPEG-1 is three standards in one (ISO/IEC 11172, 1993): video (part
   2, Mpeg1.mli), audio (part 3, Mpeg_audio.mli), and systems (part 1),
   what puts them in one file -- or one stream on a Video CD, read at a
   constant speed off the disc, where each must arrive in time, in
   small pieces, the audio never far from the pictures it goes with:

     pack      00 00 01 BA  the system clock (SCR) and the stream's rate
       system header 00 00 01 BB (the first pack's): the streams, their
                             buffers
       packet  00 00 01 E0  2 bytes of length; stuffing; the time this
                            piece of video is presented (PTS) and, for
                            pictures sent before they are shown (a P
                            before the Bs that come before it), decoded
                            (DTS); then the bytes of the video stream
       packet  00 00 01 C0  the same, of the audio stream
       packet  00 00 01 BE  padding
     pack      00 00 01 BA  ...
     end       00 00 01 B9

   A packet's stream id says what it carries: C0 to DF, one of 32 audio
   streams; E0 to EF, one of 16 video streams (a DVD's many languages
   and angles come from there). The **demultiplexer**, here, is the
   simple half: each stream's packets, their headers taken off, laid end
   to end give back the stream as its encoder wrote it (an .m1v, an
   .mp2), for its own decoder. The clock is the subtle half: a
   timestamp counts 90,000 ticks a second (a multiple of both 25 and
   30 frames a second, and of MPEG's sample rates' frames), and the
   first packets' say when each stream starts -- the audio often a few
   milliseconds before or after the video, which a player must not
   ignore.

   MPEG-2's program streams (a DVD's .vob) are the same idea with a
   longer pack header (its first bits 01, where MPEG-1's are 0010) and
   another packet header: refused here.

   References: ISO/IEC 11172-1 (1993), 2.4.3; "MPEG-1 Systems",
   https://en.wikipedia.org/wiki/MPEG_program_stream *)

type stream = {
  id : int; (* the stream id: C0 to DF audio, E0 to EF video *)
  bytes : string; (* its packets' contents, end to end *)
  first_pts : float option; (* when it starts, in seconds of the clock *)
}

(* [of_string s]: the streams, in the order their first packet comes
 * (padding and private streams left out); Failure if [s] isn't an
 * MPEG-1 system stream *)
val of_string : string -> stream list

(* the first video stream, the first audio stream *)
val video : stream list -> stream option
val audio : stream list -> stream option

(* [pts bytes]: a timestamp's 5 bytes, 33 bits among markers: 4 bits of
 * prefix, then bits 32-30, a marker bit, 15 bits and a marker, 15 bits
 * and a marker -- in ticks of 90,000 a second *)
val pts : string -> int -> int
