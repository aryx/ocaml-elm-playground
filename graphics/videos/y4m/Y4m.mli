(* Y4M (YUV4MPEG2): raw video, and nothing else.

   The format video tools hand each other uncompressed frames in
   (mjpegtools, 2001; ffmpeg, x264 and most encoders read and write it):
   a line of text saying what the frames are, then each frame as a line
   "FRAME" and its three planes of bytes (Yuv.mli), no compression,
   no index, nothing to decode but the color:

     YUV4MPEG2 W320 H240 F25:1 Ip A1:1 C420jpeg XCOLORRANGE=LIMITED\n
     FRAME\n  320 x 240 bytes of Y, 160 x 120 of Cb, 160 x 120 of Cr
     FRAME\n  ...

   The header's fields, a letter and a value each, separated by spaces:
   W and H the size; F the frame rate as a fraction (25:1, and NTSC's
   30000:1001, 29.97 a second); I the interlacing (p progressive: the
   only one read here); A the pixels' aspect; C the color sampling
   (420jpeg, 420paldv, 420mpeg2 -- 4:2:0 whose color samples sit in
   slightly different places, all read here as the same -- and 444); X
   anything else, as XCOLORRANGE=FULL or LIMITED (Yuv.mli's ranges;
   limited, video's own, when it isn't said). A frame line can carry
   fields too; they are skipped.

   **How big video is**: at 320 x 240 in 4:2:0, a frame is 115,200 bytes
   (76,800 of Y, 19,200 of each color), and at 25 frames a second, 2.9 MB
   a second; a minute of 640 x 480 at 30, 830 MB -- half of RGB's 1.6 GB,
   the 4:2:0 alone. Every other format of graphics/videos/ is a way of
   not storing all that.

   Every frame the same size, a movie read from Y4M goes to any frame at
   once (its bytes are at a known place), unlike the formats whose frames
   are differences (Movie.mli). See notes_video.md, section 2. *)

type header = {
  width : int;
  height : int;
  rate : int * int; (* frames a second, as num:den *)
  chroma : Yuv.chroma;
  range : Yuv.range;
}

(* [frame_bytes h]: a frame's planes, the "FRAME\n" line apart *)
val frame_bytes : header -> int

(* [of_string s]: the header and the frames, decoded when shown. Raises
 * Failure if [s] isn't a Y4M file, or one of a kind not read here
 * (interlaced, 4:2:2, gray). *)
val of_string : string -> header * Movie.t

(* [to_string ~rate frames]: a Y4M file of these pictures (all of the
 * same size), in 4:2:0 and the studio range unless said *)
val to_string : ?chroma:Yuv.chroma -> ?range:Yuv.range -> rate:int * int -> Rgba_image.t list -> string
