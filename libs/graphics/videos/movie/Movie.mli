(* Movie: a video as a player sees it, whatever its format (a GIF's
 * frames, and as graphics/videos/ grows, Y4M, FLI, AVI, MPEG-1) --
 * pictures, each with the time it starts, decoded when asked for.

   A minute of 320 x 240 is 1,800 pictures of 300 KB each: half a
   gigabyte decoded, so a movie is not its pictures but what gives them
   one at a time. And most formats can only go forward: a delta frame
   (FLI's, MPEG's P frames) is the one before it plus what changed, so
   frame 100 needs frame 99, which needs 98... back to the last frame
   stored whole, a **key frame**. [sequential] keeps the decoder's
   state at the last frame given, and goes on from there; it keeps the
   frame before that one too, as decoders keep their reference frames
   (and a player comparing a frame with the one before it needs):

     asked:    0   1   2   3   2   0      (seeks back)
     decoded:  0   1   2   3   -   0      the one before is kept; further
                                          back, from the start again

   which is why a player seeks to key frames, and why a movie that is
   only key frames (Motion JPEG) seeks anywhere at once.

   When a frame plays is the times array's: times.(i) is when frame i
   starts, in seconds, times.(0) = 0, and it lasts until the next one
   (the last until [duration]). A fixed rate is times.(i) = i / rate; a
   GIF's frames each have their own delay. See notes_video.md. *)

type t = {
  width : int;
  height : int;
  times : float array; (* when each frame starts, increasing, times.(0) = 0 *)
  duration : float; (* when the last frame ends *)
  frame : int -> Rgba_image.t; (* frame i, 0 <= i < Array.length times *)
}

val frame_count : t -> int

(* [of_frames frames]: pictures already decoded, each with how long it
 * shows, in seconds (a GIF's) *)
val of_frames : (Rgba_image.t * float) list -> t

(* [sequential ~width ~height ~times ~start ~next]: a movie decoded
 * forward only -- [start ()] the decoder's state before frame 0,
 * [next s] frame i from the state after frame i - 1, and the state
 * after it. Asking the frame just given again, or the one before it,
 * costs nothing; the next, one [next]; an earlier one, [start] again
 * and every frame up to it. *)
val sequential : width:int -> height:int -> times:float array -> duration:float -> start:(unit -> 's) -> next:('s -> 's * Rgba_image.t) -> t

(* [index_at m t]: the frame showing at [t] seconds (the first before
 * 0, the last after [duration]) *)
val index_at : t -> float -> int

(* [frame_at m t]: that frame *)
val frame_at : t -> float -> Rgba_image.t
