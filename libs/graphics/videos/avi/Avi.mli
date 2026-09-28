(* Avi: Microsoft's Audio Video Interleave -- the container apart from
 * the codec.

   A video file is two things easy to confuse: the **codec**, how one
   frame (or a stretch of sound) is compressed, and the **container**,
   the file that holds the compressed frames and the sound, says when
   each plays, and lets a player find frame 1000 without reading the 999
   before. AVI (Video for Windows, 1992) is the simplest famous one: a
   RIFF file, WAV's own container (Wav.mli) -- chunks of four letters, a
   size, the bytes (padded to even), and lists of chunks:

     RIFF "AVI "
       LIST "hdrl"                     the headers
         avih                          the main header (56 bytes): the
                                       microseconds a frame, the frames,
                                       the streams, the size
         LIST "strl"                   a stream, the video:
           strh                        its header: "vids", the codec
                                       ("MJPG"), the rate as rate/scale
                                       (25/1), the frames
           strf                        its format: a BITMAPINFOHEADER,
                                       the size, the codec again
         LIST "strl"                   the sound:
           strh                        "auds", rate/scale in blocks
           strf                        a WAVEFORMATEX, WAV's "fmt "
       LIST "movi"                     the data, **interleaved**:
         00dc  a frame of stream 0     so a player reading in order has
         01wb  its sound, stream 1     the picture and its sound at once
         00dc ...                      (dc: compressed video, wb: wave
                                       bytes)
       idx1                            the index: each chunk's name,
                                       whether a player can start there
                                       (a key frame), where it is and its
                                       size -- seeking without reading

   The codec here is **Motion JPEG**: every frame a JPEG of its own
   (Jpeg.mli, Jpeg_encode.mli), no frame depending on another -- what
   webcams, early digital cameras and video editors recorded, simple
   to cut anywhere, much bigger than the formats that store differences
   (Fli.mli, and MPEG). Every frame a key frame, a movie read from it
   goes to any frame at once, like Y4M's.

   The sound is PCM, WAV's samples. A player plays the sound and shows
   the frame of the sound's position: the **audio clock** drives the
   picture (notes_video.md, section 4).

   Read: RIFF walked for the headers and the "movi" list (its "rec "
   lists too), the chunks found by walking -- the index, the shortcut a
   player seeking in a big file wants, isn't needed for that, and isn't
   read; Motion JPEG video; PCM sound, 8 or 16 bits, mono or stereo,
   mixed down and resampled as Wav.of_string does. Refused: other
   codecs, compressed sound. Motion JPEG from some cameras leaves out
   the Huffman tables (the standard's are meant): not read here either.
   Written: Motion JPEG at a quality, one PCM stream, 16-bit mono,
   interleaved a frame at a time, and the index. *)

type header = {
  width : int;
  height : int;
  rate : int * int; (* frames a second as rate/scale *)
  codec : string; (* the video's four letters *)
}

(* [of_string s]: the header, the frames decoded when asked for, and
 * the sound, if any, at Signal.rate. Raises Failure if [s] isn't an
 * AVI, or one of a codec not read here. *)
val of_string : string -> header * Movie.t * Signal.t option

(* [to_string ~rate frames]: an AVI of these pictures (all of the same
 * size) as Motion JPEG at [quality] (75 by default), and [sound]
 * (mono, at Signal.rate) *)
val to_string : ?quality:int -> ?sound:Signal.t -> rate:int * int -> Rgba_image.t list -> string
