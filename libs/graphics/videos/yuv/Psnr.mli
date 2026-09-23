(* Psnr: how close a lossy picture is to the one it was made from, in
 * decibels -- the video field's common ruler.

   The **mean squared error** of the pixels' channels (red, green, blue:
   alpha isn't compared), and from it the **peak signal-to-noise
   ratio**, the largest value squared over that error, logarithmic:

     PSNR = 10 log10 (255^2 / MSE)

   Worked example: every channel off by 2, MSE 4, 42.1 dB; off by 16,
   24.0 dB. Around 30 dB a picture is watchable, past 40 it looks the
   same; two identical pictures are infinitely far from noise. It is
   not how the eye judges -- a slight shift everywhere scores worse than
   one blotch the eye catches at once -- and newer measures try to be
   (SSIM, 2004; VMAF, 2016). See notes_video.md, section 6. *)

(* [mse a b]: the mean squared error of two pictures of the same size *)
val mse : Rgba_image.t -> Rgba_image.t -> float

(* [of_mse e]: the PSNR in dB, [infinity] for 0 *)
val of_mse : float -> float

(* [psnr a b] = of_mse (mse a b) *)
val psnr : Rgba_image.t -> Rgba_image.t -> float
