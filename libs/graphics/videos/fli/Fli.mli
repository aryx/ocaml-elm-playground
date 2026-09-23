(* Fli: FLI and FLC, Autodesk Animator's movies -- a frame stored as
 * what changed since the one before.

   Jim Kent's Autodesk Animator (1989, DOS, 320 x 200 in 256 colors)
   saved its animations as FLI; Animator Pro (early 1990s) as FLC, the
   same idea for any size. Hundreds of DOS games played their cut-scenes
   from them. The idea: the first frame is stored whole, run-length
   coded, and every other one as **the pixels that changed**, the rest
   left as they were -- a still background costs nothing, a moving ball
   its outline. The first video compression most programmers met, and
   still the simplest that works.

   A file, all numbers little-endian (the PC's):

     offset  size
          0     4   the file's size
          4     2   AF11 (FLI) or AF12 (FLC)
          6     2   the frames (not counting the ring frame, below)
          8   2+2   width, height (FLI: 320 x 200)
         12     2   bits a pixel: 8
         16     4   the delay between frames: FLI in 1/70 s (the VGA's
                    refresh), FLC in milliseconds
         80   4+4   FLC: where frames 1 and 2 start
        128         the frames

     a frame:   size (4), F1FA (2), chunks (2), 8 bytes (FLC: a delay
                of its own), then its chunks
     a chunk:   size (4), type (2), then:

       COLOR_256 (4), COLOR_64 (11)   palette entries that changed:
           packets of (entries to skip, entries to set (0: 256), r g b
           each) -- in 0-255, or 0-63 as the VGA's DAC took them
       BRUN (15)   the whole frame, a line at a time, run-length coded:
           a byte (the packets, FLI's; FLC ignores it), then packets
           of a signed byte n and
             n > 0:  one byte, n times         (a run)
             n < 0:  -n bytes as they are      (literals)
       LC (12, "line compressed", FLI's delta)   lines to skip, lines
           changed (2 + 2), and each changed line: packets (1), each a
           columns-to-skip byte and a signed byte n:
             n > 0:  n bytes as they are       (the other way around
             n < 0:  one byte, -n times         from BRUN's)
       DELTA_FLC (7, FLC's delta)   the same by 16-bit words (two
           pixels at a time: faster on a 386), and whole lines skipped
           by words with their top bits 11 (-n: n lines), the last
           pixel of an odd line by 10, else a count of packets
       BLACK (13): all color 0;  COPY (16): the frame raw;  others
       (a thumbnail, PSTAMP 18): skipped

   Worked example, a line of 8 pixels, 0 0 0 0 0 0 0 0 before, 0 0 5 5
   0 0 9 0 after: in LC, 2 packets -- skip 2 columns, one byte 5 twice;
   skip 2, the one byte 9:

     02   02 FE 05   02 01 09          7 bytes for 8, and a line that
                                       didn't change, none at all

   and as BRUN, the line 7 7 7 7 1 2 3 is: 2 packets, a run of 4 sevens,
   3 literals -- 02 04 07 FD 01 02 03.

   Animator ended a file with a **ring frame**, the delta from the last
   frame back to the first, for looping without a jump; it isn't
   counted, it isn't read here, and it isn't written.

   Written here: FLC with DELTA_FLC, or FLI with LC; the palette once,
   first; a frame that didn't change is a frame of no chunks. At most
   256 colors: pictures with more need them reduced first (a median
   cut, an exercise). See notes_video.md, section 3. *)

type format = Fli | Flc

type header = { format : format; width : int; height : int; frames : int; delay : float (* seconds between frames *) }

(* [of_string s]: the header and the frames, decoded forward only, each
 * from the one before (Movie.sequential). Raises Failure if [s] isn't
 * an FLI or FLC file, or a corrupt one. *)
val of_string : string -> header * Movie.t

(* [to_string ~format ~delay frames]: an FLC (default) or FLI file of
 * these pictures, all of the same size, [delay] seconds apart. Raises
 * Invalid_argument if they have more than 256 colors. *)
val to_string : ?format:format -> delay:float -> Rgba_image.t list -> string
