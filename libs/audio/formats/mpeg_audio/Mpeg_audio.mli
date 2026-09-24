(* MPEG audio files, decoded: MP2 and MP3.

   The three layers of MPEG-1 audio (ISO/IEC 11172-3, 1993) share the
   frame (Mpeg_audio_header.mli) and the end: 32 subbands put back
   together into sound (Polyphase.mli). In between, each layer codes
   the subband samples its own way:

     file --> frames --> Layer2: the subband samples, quantized per band
                     \-> Layer3: each subband split in 18 again (the
                         MDCT), Huffman codes, and bytes borrowed from
                         the frames before
                                   |
                                   v
                     32 subbands --> Polyphase --> the sound

   Layer I (a simpler Layer II, 384 samples a frame) isn't read here,
   nor the free format, nor MP3's Xing and LAME tags (the frame count
   of a VBR file, and the encoder's delay and padding, which a gapless
   player cuts: the first frame, holding the tag, decodes to silence).

   A frame that can't be decoded (a Layer III frame whose borrowed bytes
   were cut off, at the start of a stream joined in the middle) is
   silence. *)

(* [decode s]: the first frame's header, and the samples, at its sample
 * rate (Resample.to_rate for Signal.rate's), a mono file's in both
 * channels; Error when there is no frame, or a layer we don't read *)
val decode : string -> (Mpeg_audio_header.t * Signal.stereo, string) result
