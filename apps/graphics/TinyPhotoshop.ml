(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyPhotoshop: a photograph edited dot by dot (Photoshop 1.0, Thomas
 * and John Knoll, Adobe, 1990).
 *
 *   the tools down the left: marquee, ellipse, lasso, magic wand (Shift
 *   adds to the selection, Alt takes away); pencil, paintbrush, airbrush,
 *   eraser, rubber stamp (Alt-click the place to copy from, then paint),
 *   smudge; paint bucket, gradient, eyedropper (Alt: the background
 *   colour); the two colours below them (click one to change it, x swaps
 *   them, d gives black and white back); the brush's size, hardness and
 *   opacity and the wand's tolerance along the bottom; the menus: Image's
 *   adjustments, Filter, Select, and Photos for NASA's two photographs,
 *   opened or placed as a layer; the Layers palette at the right (a
 *   click on a layer to work on it, its eye to hide it; its mode and
 *   opacity; New, Duplicate, Delete, Flatten)
 *
 * MacPaint (TinyMacPaint, beside this) kept a picture as dots, black or
 * white. Photoshop kept 24 bits a dot, and with them came the
 * operations of image processing, a laboratory's until then, as menus a
 * photographer could use: a dot's new value from its old one alone
 * (Levels, Curves, Hue/Saturation: a table of 256 entries, Lut.mli),
 * from its neighbours (Blur, Sharpen, Find Edges: a convolution,
 * Convolve.mli, Gaussian.mli, Sobel.mli), from where it moves (Image
 * Size: interpolation, Scale.mli). And the selection became a mask, a
 * byte a dot (Mask.mli): every operation is applied through it, which is
 * what lets a feathered selection's edge fade the change in.
 *
 * And the layers of Photoshop 3.0 (1994): the picture a stack of
 * pictures, each with its transparency, its opacity and its blend mode
 * (Multiply darkens, as two slides projected through each other; Screen
 * lightens, as two projectors on one screen: Blend.mli), flattened for
 * the screen by Porter and Duff's "over" (Layers.mli). The tools and
 * the menus work on the current layer; on a layer the eraser makes
 * transparent, on the background it paints the background colour.
 *
 * The adjustments and filters with dots after their name open a dialog,
 * whose sliders change a preview of the whole picture (through the
 * selection) until OK. The picture on the screen is 64 tiles, each its
 * own bitmap: a change gives new images only to the tiles whose dots it
 * changed -- the dirty rectangles of every window system, found here by
 * comparing a tile's dots with the last picture's -- so that a brush
 * stroke doesn't send the whole photograph again (in a browser, each new
 * image is encoded as a PNG).
 *
 * What it uses: libs/graphics/imaging (every operation, the blend
 * modes and the layers' flattening), our own JPEG
 * and PNG readers and writers, the photographs of photos/ (NASA's,
 * public domain, make_photos.sh), appkits/document (Undo), the File menu
 * (appkits/file_menu), and gui/'s immediate widgets.
 *
 * What it deliberately does not do: layer masks and adjustment layers
 * (Photoshop 4.0, 1996); moving a layer (the move tool); channels,
 * CMYK; the zoom and the hand;
 * text; the pen tool's paths; the history palette (Photoshop 1.0 had one
 * undo; here TinyMacPaint's Undo keeps them all).
 *
 * Exercises: Curves as a dialog where the points are dragged (Lut.curves
 * is there); the zoom tool, the tiles drawn larger; the blur and sharpen
 * tools (a brush that applies Convolve through its dabs); Crop; the
 * airbrush's flow as a slider; a Histogram palette, always open.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type tool =
  | Marquee
  | Ellipse
  | Lasso
  | Wand
  | Pencil
  | Paintbrush
  | Airbrush
  | Eraser
  | Stamp
  | Smudge
  | Bucket
  | Gradient_tool
  | Eyedropper

let tools = [ Marquee; Ellipse; Lasso; Wand; Pencil; Paintbrush; Airbrush; Eraser; Stamp; Smudge; Bucket; Gradient_tool; Eyedropper ]

let tool_name = function
  | Marquee -> "Rectangular Marquee"
  | Ellipse -> "Elliptical Marquee"
  | Lasso -> "Lasso"
  | Wand -> "Magic Wand"
  | Pencil -> "Pencil"
  | Paintbrush -> "Paintbrush"
  | Airbrush -> "Airbrush"
  | Eraser -> "Eraser"
  | Stamp -> "Rubber Stamp"
  | Smudge -> "Smudge"
  | Bucket -> "Paint Bucket"
  | Gradient_tool -> "Gradient"
  | Eyedropper -> "Eyedropper"

(* how a new selection meets the one there: Shift adds, Alt takes away *)
type combine = Replace | Add | Take_away

(* what a drag is doing, since where the mouse went down (in the
   picture's dots) *)
type drag =
  | Selecting of (float * float) * combine (* a marquee or an ellipse, from there *)
  | Lassoing of (float * float) list * combine (* the outline so far, the last point first *)
  | Stroking of (float * float) list (* a paint tool's dabs so far, in order *)
  | Dragging_gradient of float * float

type which = Foreground | Background

(* the document: its layers, the bottom one first, and the one the
   tools and the menus work on (Photoshop 3.0's) *)
type doc = { layers : Layers.layer list; current : int }

type model = {
  (* the picture, and every version of it *)
  history : doc Undo.t;
  file : File_menu.t;
  name : string;
  tool : tool;
  fg : int * int * int;
  bg : int * int * int;
  (* the brush's, and the wand's and the bucket's *)
  radius : float;
  hardness : float;
  opacity : float;
  tolerance : float;
  selection : Mask.t option;
  (* the selection when a drag began, which the drag combines with *)
  base : Mask.t option;
  drag : drag option;
  (* the picture when the drag began: a stroke is redrawn from it *)
  before : Rgba_image.t;
  (* the rubber stamp's source, set by Alt-click, and its offset from
     where the stroke began *)
  source : (float * float) option;
  dialog : dialog option;
  noise_seed : int;
  (* the layer's opacity being dragged: one Undo for the whole drag *)
  adjusting : bool;
  was : string list;
  was_down : bool;
}

(* A dialog: sliders, and what they do. [preview] computes the picture
   the sliders give, shown until OK or Cancel; [ok] what OK does (a
   colour picker changes a colour, not the picture). The last preview
   is kept with its values: computed again only when a slider moved. *)
and dialog = {
  title : string;
  sliders : (string * float * float * (float -> string)) list; (* label, from, to, how its value reads *)
  values : float list;
  preview : (float list -> Rgba_image.t -> Rgba_image.t) option;
  ok : float list -> model -> model;
  histogram : bool;
  cache : (float list * Layers.layer list) option; (* the layers with the current one previewed *)
}

let decode (jpg : string) : Rgba_image.t = Jpeg.decode jpg
let blue_marble = lazy (decode Photos.blue_marble_jpg)

let start (name : string) (img : Rgba_image.t) : model =
  {
    history = Undo.start { layers = [ Layers.make "Background" img ]; current = 0 };
    file = File_menu.start;
    name;
    tool = Paintbrush;
    fg = (0, 0, 0);
    bg = (255, 255, 255);
    radius = 8.;
    hardness = 0.5;
    opacity = 1.;
    tolerance = 32.;
    selection = None;
    base = None;
    drag = None;
    before = img;
    source = None;
    dialog = None;
    noise_seed = 1;
    adjusting = false;
    was = [];
    was_down = false;
  }

let initial = lazy (start "The Blue Marble" (Lazy.force blue_marble))
let doc (m : model) : doc = Undo.now m.history
let layers (m : model) : Layers.layer list = (doc m).layers
let layer (m : model) : Layers.layer = List.nth (layers m) (doc m).current

(* the current layer's picture: what the tools and the menus change *)
let picture (m : model) : Rgba_image.t = (layer m).image

(* the layers with the current one's picture replaced *)
let with_image (d : doc) (img : Rgba_image.t) : Layers.layer list = List.mapi (fun i (l : Layers.layer) -> if i = d.current then { l with image = img } else l) d.layers
let record_doc ~name (d : doc) (m : model) = { m with history = Undo.record ~name d m.history }
let record ~name img (m : model) = record_doc ~name { (doc m) with layers = with_image (doc m) img } m
let amend img (m : model) = { m with history = Undo.amend { (doc m) with layers = with_image (doc m) img } m.history }

(* the whole picture as the screen shows it: the layers flattened,
   computed again only for new layers (the same list, the same
   picture: the history's documents are never changed in place) *)
let flat : (Layers.layer list * Rgba_image.t) option ref = ref None

let flatten (ls : Layers.layer list) : Rgba_image.t =
  match !flat with
  | Some (ls', img) when ls' == ls -> img
  | _ ->
      let img = Layers.flatten ls in
      flat := Some (ls, img);
      img

let composite (m : model) : Rgba_image.t = flatten (layers m)

(*****************************************************************************)
(* The picture's window *)
(*****************************************************************************)

(* the area the picture is shown in, its centre and side: a picture
   larger than it is shown smaller *)
let area_x = 30.
let area_y = 30.
let area = 420.

let scale (img : Rgba_image.t) : float = Float.min 1. (area /. float_of_int (max img.width img.height))

(* the picture's top-left corner on the screen *)
let origin (img : Rgba_image.t) : float * float =
  let s = scale img in
  (area_x -. (float_of_int img.width *. s /. 2.), area_y +. (float_of_int img.height *. s /. 2.))

(* the point of the picture under a point of the screen, in dots *)
let dot_at (img : Rgba_image.t) ((x, y) : float * float) : float * float =
  let s = scale img and left, top = origin img in
  ((x -. left) /. s, (top -. y) /. s)

let on_picture (img : Rgba_image.t) ((x, y) : float * float) = x >= 0. && y >= 0. && x < float_of_int img.width && y < float_of_int img.height

(*****************************************************************************)
(* Operations, through the selection *)
(*****************************************************************************)

(* an operation on the whole picture, kept only where the selection
   says (Composite.mli) *)
let through (m : model) (f : Rgba_image.t -> Rgba_image.t) (img : Rgba_image.t) : Rgba_image.t =
  let after = f img in
  match m.selection with Some sel -> Composite.through sel ~before:img ~after | None -> after

let apply ~name (f : Rgba_image.t -> Rgba_image.t) (m : model) : model = record ~name (through m f (picture m)) m

(* the operations that change the picture's shape: the selection goes *)
let reshape ~name (f : Rgba_image.t -> Rgba_image.t) (m : model) : model =
  let d = doc m in
  { (record_doc ~name { d with layers = List.map (fun (l : Layers.layer) -> { l with image = f l.image }) d.layers } m) with selection = None }

let combine (mode : combine) (base : Mask.t option) (fresh : Mask.t) : Mask.t option =
  let result =
    match (mode, base) with
    | Add, Some b -> Mask.union b fresh
    | Take_away, Some b -> Mask.subtract b fresh
    | Take_away, None -> Mask.empty fresh.width fresh.height
    | _ -> fresh
  in
  if Mask.is_empty result then None else Some result

(*****************************************************************************)
(* The dialogs *)
(*****************************************************************************)

let int_value v = string_of_int (int_of_float (Float.round v))
let percent v = Printf.sprintf "%d%%" (int_of_float (Float.round v))
let one_decimal v = Printf.sprintf "%.1f" v
let ints (vs : float list) = List.map (fun v -> int_of_float (Float.round v)) vs

(* a dialog whose OK applies its preview's operation, through the
   selection *)
let adjustment ?(histogram = false) ~title sliders values (f : float list -> Rgba_image.t -> Rgba_image.t) : dialog =
  { title; sliders; values; preview = Some f; histogram; cache = None; ok = (fun vs m -> apply ~name:title (f vs) m) }

let method_name v = match int_of_float (Float.round v) with 0 -> "Nearest" | 1 -> "Bilinear" | _ -> "Bicubic"

let image_size () : dialog =
  let resize vs (img : Rgba_image.t) : Rgba_image.t =
    match vs with
    | [ pct; meth ] ->
        let method_ : Scale.method_ = match int_of_float (Float.round meth) with 0 -> Nearest | 1 -> Bilinear | _ -> Bicubic in
        let size n = max 1 (int_of_float (Float.round (float_of_int n *. pct /. 100.))) in
        Scale.resize method_ ~width:(size img.width) ~height:(size img.height) img
    | _ -> img
  in
  {
    title = "Image Size";
    sliders = [ ("Size", 10., 200., percent); ("Resample", 0., 2., method_name) ];
    values = [ 50.; 2. ];
    (* the preview shows the picture resized, at the size it will be *)
    preview = Some resize;
    ok = (fun vs m -> reshape ~name:"Image Size" (resize vs) m);
    histogram = false;
    cache = None;
  }

let colour_picker (which : which) (m : model) : dialog =
  let r, g, b = match which with Foreground -> m.fg | Background -> m.bg in
  {
    title = (match which with Foreground -> "Foreground Colour" | Background -> "Background Colour");
    sliders = [ ("Red", 0., 255., int_value); ("Green", 0., 255., int_value); ("Blue", 0., 255., int_value) ];
    values = List.map float_of_int [ r; g; b ];
    preview = None;
    ok =
      (fun vs m ->
        match ints vs with
        | [ r; g; b ] -> ( match which with Foreground -> { m with fg = (r, g, b) } | Background -> { m with bg = (r, g, b) })
        | _ -> m);
    histogram = false;
    cache = None;
  }

(* the menus' items with dots: their dialogs *)
let dialog_for (item : string) (m : model) : dialog option =
  let two f = function [ a; b ] -> f a b | _ -> Fun.id in
  let three f = function [ a; b; c ] -> f a b c | _ -> Fun.id in
  let one f = function [ a ] -> f a | _ -> Fun.id in
  match item with
  | "Levels..." ->
      Some
        (adjustment ~histogram:true ~title:"Levels"
           [ ("Black", 0., 254., int_value); ("Gamma", 0.2, 3., (fun v -> Printf.sprintf "%.2f" v)); ("White", 1., 255., int_value) ]
           [ 0.; 1.; 255. ]
           (three (fun b g w -> Lut.apply (Lut.levels ~black:(int_of_float b) ~white:(int_of_float (Float.max (b +. 1.) w)) ~gamma:g))))
  | "Brightness/Contrast..." ->
      Some
        (adjustment ~title:"Brightness/Contrast" [ ("Brightness", -100., 100., int_value); ("Contrast", -100., 100., int_value) ] [ 0.; 0. ]
           (two (fun b c -> Lut.apply (Lut.brightness_contrast ~brightness:(int_of_float b) ~contrast:(int_of_float c)))))
  | "Hue/Saturation..." ->
      Some
        (adjustment ~title:"Hue/Saturation"
           [ ("Hue", -180., 180., int_value); ("Saturation", -100., 100., int_value); ("Lightness", -100., 100., int_value) ]
           [ 0.; 0.; 0. ]
           (three (fun h s l -> Hsl.hue_saturation ~hue:(Float.round h) ~saturation:(Float.round s) ~lightness:(Float.round l))))
  | "Curves..." ->
      (* a curve through one point, the middle's: the simplest Curves *)
      Some
        (adjustment ~title:"Curves" [ ("Input", 1., 254., int_value); ("Output", 0., 255., int_value) ] [ 128.; 160. ]
           (two (fun i o -> Lut.apply (Lut.curves [ (int_of_float i, int_of_float o) ]))))
  | "Posterize..." -> Some (adjustment ~title:"Posterize" [ ("Levels", 2., 16., int_value) ] [ 4. ] (one (fun n -> Lut.apply (Lut.posterize (int_of_float (Float.round n))))))
  | "Threshold..." -> Some (adjustment ~title:"Threshold" [ ("Level", 1., 255., int_value) ] [ 128. ] (one (fun l -> Lut.threshold (int_of_float l))))
  | "Gaussian Blur..." -> Some (adjustment ~title:"Gaussian Blur" [ ("Radius", 0.1, 10., one_decimal) ] [ 2. ] (one (fun r -> Gaussian.blur ~radius:(Float.round (r *. 10.) /. 10.))))
  | "Unsharp Mask..." ->
      Some
        (adjustment ~title:"Unsharp Mask"
           [ ("Amount", 1., 500., percent); ("Radius", 0.1, 10., one_decimal); ("Threshold", 0., 255., int_value) ]
           [ 100.; 1.; 0. ]
           (three (fun a r t -> Gaussian.unsharp ~amount:(Float.round a) ~radius:(Float.round (r *. 10.) /. 10.) ~threshold:(int_of_float t))))
  | "Median..." -> Some (adjustment ~title:"Median" [ ("Radius", 1., 4., int_value) ] [ 1. ] (one (fun r -> Median.median ~radius:(int_of_float (Float.round r)))))
  | "Add Noise..." ->
      Some (adjustment ~title:"Add Noise" [ ("Amount", 1., 100., int_value) ] [ 20. ] (one (fun a img -> Add_noise.add ~amount:(int_of_float a) ~seed:m.noise_seed img)))
  | "Feather..." ->
      Some
        {
          title = "Feather";
          sliders = [ ("Radius", 0.5, 20., one_decimal) ];
          values = [ 3. ];
          preview = None;
          ok = (fun vs m -> match (vs, m.selection) with [ r ], Some sel -> { m with selection = Some (Mask.feather ~radius:r sel) } | _ -> m);
          histogram = false;
          cache = None;
        }
  | "Image Size..." -> Some (image_size ())
  | _ -> None

(*****************************************************************************)
(* The menus *)
(*****************************************************************************)

let menu_edit = [ "Edit"; "Undo"; "Redo"; "Fill"; "Clear" ]
let menu_select = [ "Select"; "All"; "None"; "Inverse"; "Feather..." ]

let menu_image =
  [ "Image"; "Levels..."; "Auto Levels"; "Curves..."; "Brightness/Contrast..."; "Hue/Saturation..."; "Invert"; "Desaturate"; "Posterize...";
    "Threshold..."; "Image Size..."; "Flip Horizontal"; "Flip Vertical"; "Rotate 90" ]

let menu_filter =
  [ "Filter"; "Blur"; "Blur More"; "Gaussian Blur..."; "Sharpen"; "Sharpen More"; "Unsharp Mask..."; "Find Edges"; "Emboss"; "Median...";
    "Add Noise..." ]

let menu_photos = [ "Photos"; "Blue Marble"; "Aldrin"; "Place Marble"; "Place Aldrin"; "Export PNG"; "Export JPEG" ]

let bar_widths () : float list =
  List.map (fun items -> Float.max 80. (fst (Gui.menu_size items) +. 6.)) [ File_menu.items; menu_edit; menu_select; menu_image; menu_filter; menu_photos ]

(* File > Place: a photograph as a new layer on top, at the picture's size *)
let place (name : string) (jpg : string) (m : model) : model =
  let d = doc m and img = picture m in
  let photo = Scale.resize Bilinear ~width:img.width ~height:img.height (decode jpg) in
  let layers = d.layers @ [ Layers.make name photo ] in
  { (record_doc ~name:"Place" { layers; current = List.length layers - 1 } m) with selection = None }

let open_photo (name : string) (jpg : string) (m : model) : model =
  { (start name (decode jpg)) with file = m.file; fg = m.fg; bg = m.bg; radius = m.radius; hardness = m.hardness; opacity = m.opacity; tolerance = m.tolerance }

let export (caps : File_menu.caps) (m : model) (extension : string) : model =
  let img = composite m in
  let data = if extension = ".png" then Png.encode img else Jpeg_encode.encode ~quality:90 img in
  let file = String.map (fun c -> if c = ' ' then '_' else Char.lowercase_ascii c) m.name ^ extension in
  Playground_platform.export caps file data;
  m

(* what a menu's item does: at once, or a dialog first *)
let command (caps : File_menu.caps) (item : string) (m : model) : model =
  match dialog_for item m with
  | Some d -> { m with dialog = Some d }
  | None -> (
      match item with
      | "Undo" -> { m with history = Undo.undo m.history; selection = None }
      | "Redo" -> { m with history = Undo.redo m.history; selection = None }
      | "Fill" -> apply ~name:"Fill" (fun (img : Rgba_image.t) -> Composite.fill (Mask.all img.width img.height) m.fg img) m
      | "Clear" -> apply ~name:"Clear" (fun (img : Rgba_image.t) -> Composite.fill (Mask.all img.width img.height) m.bg img) m
      | "All" -> let img = picture m in { m with selection = Some (Mask.all img.width img.height) }
      | "None" -> { m with selection = None }
      | "Inverse" -> ( match m.selection with Some s -> { m with selection = combine Replace None (Mask.invert s) } | None -> m)
      | "Auto Levels" ->
          let black, white = Histogram.auto_levels (Histogram.compute (picture m)) in
          apply ~name:"Auto Levels" (Lut.apply (Lut.levels ~black ~white ~gamma:1.)) m
      | "Invert" -> apply ~name:"Invert" (Lut.apply Lut.invert) m
      | "Desaturate" -> apply ~name:"Desaturate" Lut.desaturate m
      | "Flip Horizontal" -> reshape ~name:"Flip" Scale.flip_horizontal m
      | "Flip Vertical" -> reshape ~name:"Flip" Scale.flip_vertical m
      | "Rotate 90" -> reshape ~name:"Rotate" Scale.rotate_90 m
      | "Blur" -> apply ~name:"Blur" (Convolve.apply Convolve.blur) m
      | "Blur More" -> apply ~name:"Blur More" (Convolve.apply Convolve.blur_more) m
      | "Sharpen" -> apply ~name:"Sharpen" (Convolve.apply Convolve.sharpen) m
      | "Sharpen More" -> apply ~name:"Sharpen More" (Convolve.apply Convolve.sharpen_more) m
      | "Find Edges" -> apply ~name:"Find Edges" Sobel.find_edges m
      | "Emboss" -> apply ~name:"Emboss" (Convolve.apply Convolve.emboss) m
      | "Blue Marble" -> open_photo "The Blue Marble" Photos.blue_marble_jpg m
      | "Aldrin" -> open_photo "Aldrin on the Moon" Photos.aldrin_jpg m
      | "Place Marble" -> place "Blue Marble" Photos.blue_marble_jpg m
      | "Place Aldrin" -> place "Aldrin" Photos.aldrin_jpg m
      | "Export PNG" -> export caps m ".png"
      | "Export JPEG" -> export caps m ".jpg"
      | _ -> m)

(*****************************************************************************)
(* The tools *)
(*****************************************************************************)

let brush (m : model) : Brush.t = { Brush.radius = m.radius; hardness = m.hardness; opacity = m.opacity }

(* a stroke of dabs, painted on the picture it began on: the whole
   stroke again at each frame, so that it is one "Undo" and its opacity
   the same where its dabs overlap (Brush.mli) *)
let stroke (m : model) (points : (float * float) list) : Rgba_image.t =
  let selection = m.selection and before = m.before in
  match m.tool with
  | Pencil -> Brush.paint { Brush.radius = 0.8; hardness = 1.; opacity = 1. } m.fg ?selection before points
  | Paintbrush -> Brush.paint (brush m) m.fg ?selection before points
  | Airbrush -> Brush.airbrush (brush m) m.fg ~flow:0.08 ?selection before points
  | Eraser when (doc m).current = 0 -> Brush.paint { (brush m) with hardness = 1. } m.bg ?selection before points
  | Eraser ->
      (* on a layer, erasing is making transparent *)
      let clear = Pixels.map (fun r g b _ -> (r, g, b, 0)) before in
      let stroke = Brush.stroke_mask { (brush m) with hardness = 1. } before.width before.height points in
      let stroke = match selection with Some s -> Mask.intersect stroke s | None -> stroke in
      Composite.through stroke ~before ~after:clear
  | Stamp -> (
      match (m.source, points) with
      | Some (sx, sy), (x0, y0) :: _ ->
          let offset = (int_of_float (Float.round (sx -. x0)), int_of_float (Float.round (sy -. y0))) in
          Brush.stamp (brush m) ~source:before ~offset ?selection before points
      | _ -> before)
  | Smudge -> Brush.smudge (brush m) ~strength:0.6 ?selection before points
  | _ -> before

let mode_of (computer : computer) : combine =
  if Set_.mem "Alt" computer.keyboard.keys then Take_away else if computer.keyboard.kshift then Add else Replace

let pixel (img : Rgba_image.t) ((x, y) : float * float) : int * int * int =
  let x = int_of_float x and y = int_of_float y in
  (Pixels.get img x y 0, Pixels.get img x y 1, Pixels.get img x y 2)

(* the mouse went down on the picture, at [p] *)
let press (computer : computer) (m : model) (p : float * float) : model =
  let img = picture m in
  let alt = Set_.mem "Alt" computer.keyboard.keys in
  let x, y = p in
  match m.tool with
  | Marquee | Ellipse -> { m with drag = Some (Selecting (p, mode_of computer)); base = m.selection }
  | Lasso -> { m with drag = Some (Lassoing ([ p ], mode_of computer)); base = m.selection }
  | Wand ->
      let fresh = Mask.wand ~tolerance:(int_of_float m.tolerance) img (int_of_float x) (int_of_float y) in
      { m with selection = combine (mode_of computer) m.selection fresh }
  | Bucket ->
      let area = Mask.wand ~tolerance:(int_of_float m.tolerance) img (int_of_float x) (int_of_float y) in
      let area = match m.selection with Some s -> Mask.intersect area s | None -> area in
      record ~name:"Fill" (Composite.fill area m.fg img) m
  (* the colour as seen: the layers flattened *)
  | Eyedropper -> if alt then { m with bg = pixel (composite m) p } else { m with fg = pixel (composite m) p }
  | Gradient_tool -> { (record ~name:"Gradient" img m) with drag = Some (Dragging_gradient (x, y)); before = img }
  | Stamp when alt -> { m with source = Some p }
  | Stamp when m.source = None -> m
  | Pencil | Paintbrush | Airbrush | Eraser | Stamp | Smudge ->
      let m = { m with before = img; drag = Some (Stroking [ p ]) } in
      record ~name:(tool_name m.tool) (stroke m [ p ]) m

(* the mouse, still down, is at [p] *)
let drag (m : model) (p : float * float) : model =
  let img = picture m in
  match m.drag with
  | Some (Selecting ((x0, y0), mode)) ->
      let corner (x, y) = (int_of_float (Float.round x), int_of_float (Float.round y)) in
      let shape = if m.tool = Ellipse then Mask.ellipse else Mask.rectangle in
      { m with selection = combine mode m.base (shape img.width img.height (corner (x0, y0)) (corner p)) }
  | Some (Lassoing (points, mode)) -> (
      match points with
      | (lx, ly) :: _ when Float.abs (fst p -. lx) +. Float.abs (snd p -. ly) < 2. -> m
      | _ -> { m with drag = Some (Lassoing (p :: points, mode)) })
  | Some (Stroking points) ->
      let last = List.nth points (List.length points - 1) in
      (* the dabs from the last one to the mouse; the airbrush lays one
         more even where the mouse stays: it keeps spraying *)
      let more = match Brush.spacing (brush m) last p with _ :: rest -> rest | [] -> [] in
      let more = if more = [] && m.tool = Airbrush then [ p ] else more in
      if more = [] then m
      else
        let points = points @ more in
        { (amend (stroke m points) m) with drag = Some (Stroking points) }
  | Some (Dragging_gradient (x0, y0)) -> amend (Gradient.linear ?selection:m.selection (x0, y0) p m.fg m.bg m.before) m
  | None -> m

let release (m : model) : model =
  let img = picture m in
  let m =
    match m.drag with
    | Some (Lassoing (points, mode)) when List.length points >= 3 -> { m with selection = combine mode m.base (Mask.polygon img.width img.height points) }
    | Some (Selecting (_, Replace)) -> (
        (* a click, no drag: nothing selected *)
        match Option.bind m.selection Mask.bounds with Some (a, b, c, d) when c - a <= 1 && d - b <= 1 -> { m with selection = None } | _ -> m)
    | _ -> m
  in
  { m with drag = None }

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

let tool_size = 40.

(* two columns, down the left, as Photoshop 1.0 had them *)
let tool_box i : Widget.box = { Widget.x = -445. +. (float_of_int (i mod 2) *. (tool_size +. 2.)); y = 380. -. (float_of_int (i / 2) *. (tool_size +. 2.)); w = tool_size; h = tool_size }
let fg_box : Widget.box = { Widget.x = -435.; y = 70.; w = 34.; h = 34. }
let bg_box : Widget.box = { Widget.x = -413.; y = 48.; w = 34.; h = 34. }

(* the menu bar's titles, side by side, each box as wide as its menu's
   longest item: the toolkit's dropdown is as wide as its title *)
let menu_box (widths : float list) i : Widget.box =
  let before = List.fold_left ( +. ) 0. (List.filteri (fun j _ -> j < i) widths) in
  let w = List.nth widths i in
  { Widget.x = -490. +. before +. (w /. 2.); y = 475.; w = w -. 6.; h = 30. }

(* the options along the bottom: the brush and the tolerance *)
let option_box i : Widget.box = { Widget.x = -330. +. (float_of_int i *. 220.); y = -320.; w = 150.; h = 24. }

(* a dialog's panel, right of the picture *)
let panel_x = 372.
let panel_w = 240.
let slider_box i : Widget.box = { Widget.x = panel_x; y = 150. -. (float_of_int i *. 70.); w = 180.; h = 24. }
let ok_box : Widget.box = { Widget.x = panel_x -. 55.; y = -130.; w = 80.; h = 30. }
let cancel_box : Widget.box = { Widget.x = panel_x +. 55.; y = -130.; w = 80.; h = 30. }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the dialog open: its sliders, its preview kept up to date, OK or
   Cancel *)
let dialog_update (computer : computer) (m : model) (d : dialog) : model =
  let values = List.mapi (fun i ((_, from, to_, _), v) -> Gui.slider_in computer (slider_box i) ~from ~to_ v) (List.combine d.sliders d.values) in
  let d = { d with values } in
  let d =
    match (d.preview, d.cache) with
    | Some f, cache when (match cache with Some (vs, _) -> vs <> values | None -> true) ->
        { d with cache = Some (values, with_image (doc m) (through m (f values) (picture m))) }
    | _ -> d
  in
  if Gui.button_in computer ok_box "OK" then d.ok d.values { m with dialog = None }
  else if Gui.button_in computer cancel_box "Cancel" then { m with dialog = None }
  else { m with dialog = Some d }

(* The Layers palette, Photoshop 3.0's: the layers top first, each with
   its eye (shown or not) and its name, the current one highlighted; its
   blend mode and opacity above them; New, Duplicate, Delete, Flatten
   below. A click on a row makes that layer the current one. *)
let mode_box : Widget.box = { Widget.x = panel_x; y = 255.; w = 190.; h = 28. }
let opacity_box : Widget.box = { Widget.x = panel_x; y = 195.; w = 190.; h = 24. }
let row_box i : Widget.box = { Widget.x = panel_x; y = 145. -. (float_of_int i *. 34.); w = 226.; h = 30. }
let eye_box i : Widget.box = { (row_box i) with x = panel_x -. 96.; w = 28. }
let rows = 7
let layer_buttons = [ "New"; "Duplicate"; "Delete"; "Flatten" ]
let layer_button_box i : Widget.box = { Widget.x = panel_x -. 57. +. (float_of_int (i mod 2) *. 114.); y = -115. -. (float_of_int (i / 2) *. 40.); w = 104.; h = 32. }

(* the current layer changed by [f] *)
let change_layer (f : Layers.layer -> Layers.layer) (d : doc) : doc = { d with layers = List.mapi (fun i l -> if i = d.current then f l else l) d.layers }

let layer_command (label : string) (m : model) : model =
  let d = doc m in
  let n = List.length d.layers in
  let insert_above (l : Layers.layer) = List.concat (List.mapi (fun i x -> if i = d.current then [ x; l ] else [ x ]) d.layers) in
  let img = picture m in
  match label with
  | "New" -> record_doc ~name:"New Layer" { layers = insert_above (Layers.transparent (Printf.sprintf "Layer %d" n) img.width img.height); current = d.current + 1 } m
  | "Duplicate" -> record_doc ~name:"Duplicate Layer" { layers = insert_above { (layer m) with name = (layer m).name ^ " copy" }; current = d.current + 1 } m
  | "Delete" when n > 1 -> record_doc ~name:"Delete Layer" { layers = List.filteri (fun i _ -> i <> d.current) d.layers; current = max 0 (d.current - 1) } m
  | "Flatten" ->
      (* over white, as Photoshop's background is: no transparency left *)
      let white = Composite.fill (Mask.all img.width img.height) (255, 255, 255) (Rgba_image.create ~width:img.width ~height:img.height) in
      record_doc ~name:"Flatten" { layers = [ Layers.make "Background" (Layers.flatten (Layers.make "white" white :: d.layers)) ]; current = 0 } m
  | _ -> m

let palette_update (computer : computer) (m : model) : model =
  let d = doc m and cur = layer m in
  let names = List.map Blend.name Blend.modes in
  let index = let rec go i = function [] -> 0 | x :: rest -> if x = cur.mode then i else go (i + 1) rest in go 0 Blend.modes in
  let chosen = Gui.menu_in computer mode_box names index in
  let m = if chosen <> index then record_doc ~name:"Blending Mode" (change_layer (fun l -> { l with mode = List.nth Blend.modes chosen }) d) m else m in
  let v = Gui.slider_in computer opacity_box ~from:0. ~to_:100. ((layer m).opacity *. 100.) in
  let m =
    if Float.abs (v -. ((layer m).opacity *. 100.)) > 0.01 then
      let nd = change_layer (fun l -> { l with opacity = v /. 100. }) (doc m) in
      if m.adjusting then { m with history = Undo.amend nd m.history } else { (record_doc ~name:"Opacity" nd m) with adjusting = true }
    else m
  in
  let m = if computer.mouse.mdown then m else { m with adjusting = false } in
  let m = List.fold_left (fun m (i, label) -> if Gui.button_in computer (layer_button_box i) label then layer_command label m else m) m (List.mapi (fun i l -> (i, l)) layer_buttons) in
  (* a click on a row: its eye, or the row itself *)
  let mouse = computer.mouse in
  if mouse.mdown && (not m.was_down) && not (Gui.modal ()) then
    let d = doc m in
    let n = List.length d.layers in
    let rec find i =
      if i >= min n rows then m
      else
        let index = n - 1 - i in
        if Widget.contains (eye_box i) mouse.mx mouse.my then
          let l = List.nth d.layers index in
          record_doc ~name:(if l.visible then "Hide Layer" else "Show Layer")
            { d with layers = List.mapi (fun j (x : Layers.layer) -> if j = index then { x with visible = not x.visible } else x) d.layers } m
        else if Widget.contains (row_box i) mouse.mx mouse.my then { m with history = Undo.amend { d with current = index } m.history }
        else find (i + 1)
    in
    find 0
  else m

let keyboard (caps : File_menu.caps) (computer : computer) (m : model) : model =
  let now = Set_.elements computer.keyboard.keys in
  let pressed key = List.mem key now && not (List.mem key m.was) in
  let m =
    if List.mem "Control" now then
      if pressed "z" then command caps "Undo" m
      else if pressed "y" then command caps "Redo" m
      else if pressed "a" then { m with selection = Some (Mask.all (picture m).width (picture m).height) }
      else if pressed "d" then { m with selection = None }
      else m
    else if m.dialog <> None then m
    else if pressed "x" then { m with fg = m.bg; bg = m.fg }
    else if pressed "d" then { m with fg = (0, 0, 0); bg = (255, 255, 255) }
    else if pressed "Backspace" || pressed "Delete" then command caps "Clear" m
    else m
  in
  { m with was = now }

(* a document is its picture, saved by the File menu as it is *)
let kind = { File_menu.magic = "TinyPhotoshop 2"; extension = ".photo" }

let reopened (r : doc File_menu.result) (m : model) : model =
  match r with
  | File_menu.Nothing -> m
  | File_menu.New ->
      let img = Composite.fill (Mask.all 400 400) m.bg (Rgba_image.create ~width:400 ~height:400) in
      let img = Pixels.map (fun r g b _ -> (r, g, b, 255)) img in
      { (start "Untitled" img) with file = m.file }
  | File_menu.Opened d -> { (start (File_menu.title m.file) (List.hd d.layers).image) with file = m.file; history = Undo.start d }

let update (caps : File_menu.caps) (computer : computer) (m : model) : model =
  let current () = doc m in
  if File_menu.busy m.file then
    let file, r = File_menu.dialog caps kind computer ~current m.file in
    reopened r { m with file; was_down = computer.mouse.mdown; was = Set_.elements computer.keyboard.keys }
  else
    let mouse = computer.mouse in
    (* the menus first, so that an open one gets the click *)
    let m =
      let file, r = File_menu.menu_in caps kind computer (menu_box (bar_widths ()) 0) ~current m.file in
      reopened r { m with file }
    in
    let menu i items m =
      let chosen = Gui.menu_in computer (menu_box (bar_widths ()) i) items 0 in
      if chosen > 0 && m.dialog = None then command caps (List.nth items chosen) m else m
    in
    let m = m |> menu 1 menu_edit |> menu 2 menu_select |> menu 3 menu_image |> menu 4 menu_filter |> menu 5 menu_photos in
    let m =
      match m.dialog with
      | Some d -> dialog_update computer m d
      | None ->
          let m = palette_update computer m in
          (* the options, always there *)
          let slide i ~from ~to_ v = Gui.slider_in computer (option_box i) ~from ~to_ v in
          let m = { m with radius = slide 0 ~from:1. ~to_:50. m.radius } in
          let m = { m with hardness = slide 1 ~from:0. ~to_:1. m.hardness } in
          let m = { m with opacity = slide 2 ~from:0.05 ~to_:1. m.opacity } in
          let m = { m with tolerance = slide 3 ~from:0. ~to_:128. m.tolerance } in
          if Gui.modal () then m
          else
            let img = picture m in
            let p = dot_at img (mouse.mx, mouse.my) in
            let pressed = mouse.mdown && not m.was_down in
            let hit box = List.find_opt (fun (i, _) -> Widget.contains (box i) mouse.mx mouse.my) in
            match (m.drag, pressed) with
            | Some _, _ when mouse.mdown -> drag m p
            | Some _, _ -> release m
            | None, true when on_picture img p -> press computer m p
            | None, true -> (
                if Widget.contains fg_box mouse.mx mouse.my then { m with dialog = Some (colour_picker Foreground m) }
                else if Widget.contains bg_box mouse.mx mouse.my then { m with dialog = Some (colour_picker Background m) }
                else match hit tool_box (List.mapi (fun i t -> (i, t)) tools) with Some (_, tool) -> { m with tool } | None -> m)
            | None, false -> m
    in
    let m = keyboard caps computer m in
    { m with was_down = mouse.mdown; noise_seed = (if m.dialog = None then m.noise_seed + 1 else m.noise_seed) }

(*****************************************************************************)
(* The picture as tiles *)
(*****************************************************************************)

let tile = 50

(* the dots of a rectangle of the picture, as an image of their own *)
let sub (img : Rgba_image.t) (x0 : int) (y0 : int) (w : int) (h : int) : Rgba_image.t =
  let t = Rgba_image.create ~width:w ~height:h in
  for y = 0 to h - 1 do
    Bigarray.Array1.blit (Bigarray.Array1.sub img.rgba (4 * (((y0 + y) * img.width) + x0)) (4 * w)) (Bigarray.Array1.sub t.rgba (4 * y * w) (4 * w))
  done;
  t

let same_dots (a : Rgba_image.t) (b : Rgba_image.t) (x0 : int) (y0 : int) (w : int) (h : int) : bool =
  let rec row y = y >= h || (Bigarray.Array1.sub a.rgba (4 * (((y0 + y) * a.width) + x0)) (4 * w) = Bigarray.Array1.sub b.rgba (4 * (((y0 + y) * b.width) + x0)) (4 * w) && row (y + 1)) in
  row 0

(* The tiles of the last picture shown, kept: a new picture gets new
   tiles only where its dots differ from the last one's, the others are
   the same images as before -- which the backends have converted already
   (on the web, encoded as a PNG) and draw again for nothing. *)
let shown : (Rgba_image.t * ((int * int * int * int) * Rgba_image.t) list) option ref = ref None

let tiles (img : Rgba_image.t) : ((int * int * int * int) * Rgba_image.t) list =
  match !shown with
  | Some (last, ts) when last == img -> ts
  | last ->
      let rects =
        List.concat
          (List.init ((img.height + tile - 1) / tile) (fun j ->
               List.init ((img.width + tile - 1) / tile) (fun i ->
                   let x = i * tile and y = j * tile in
                   (x, y, min tile (img.width - x), min tile (img.height - y)))))
      in
      let ts =
        List.map
          (fun ((x, y, w, h) as r) ->
            match last with
            | Some (old, olds) when old.width = img.width && old.height = img.height && same_dots old img x y w h -> (r, List.assoc r olds)
            | _ -> (r, sub img x y w h))
          rects
      in
      shown := Some (img, ts);
      ts

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let colour (r, g, b) = rgb r g b

(* the marching ants: the selection's edges, dashes moving along them *)
let ants_cache : (Mask.t * ((int * int) * (int * int)) list) option ref = ref None

let ants (computer : computer) (img : Rgba_image.t) (sel : Mask.t) : shape list =
  let edges = match !ants_cache with Some (s, e) when s == sel -> e | _ -> let e = Mask.edges sel in ants_cache := Some (sel, e); e in
  let s = scale img and left, top = origin img in
  let phase = int_of_float (spin 1. computer.time /. 45.) in
  List.map
    (fun ((x0, y0), (x1, y1)) ->
      let c = if (x0 + y0 + phase) mod 8 < 4 then black else white in
      let cx = left +. (float_of_int (x0 + x1) /. 2. *. s) and cy = top -. (float_of_int (y0 + y1) /. 2. *. s) in
      if y0 = y1 then rectangle c (s +. 0.5) 1.5 |> move cx cy else rectangle c 1.5 (s +. 0.5) |> move cx cy)
    edges

(* a tool's icon, drawn with shapes *)
let icon (b : Widget.box) (t : tool) ~ink ~paper : shape list =
  let at dx dy s = s |> move (b.x +. dx) (b.y +. dy) in
  match t with
  | Marquee ->
      List.concat_map (fun i -> let d = -12. +. (float_of_int i *. 6.) in [ at d 11. (rectangle ink 3. 2.); at d (-11.) (rectangle ink 3. 2.); at 12. d (rectangle ink 2. 3.); at (-12.) d (rectangle ink 2. 3.) ]) [ 0; 1; 2; 3; 4 ]
  | Ellipse -> List.init 12 (fun i -> let a = float_of_int i *. 30. in at (13. *. cos (a *. Float.pi /. 180.)) (9. *. sin (a *. Float.pi /. 180.)) (rectangle ink 2.5 2.5))
  | Lasso -> [ at 0. 3. (oval ink 26. 16.); at 0. 3. (oval paper 22. 12.); at (-8.) (-9.) (rectangle ink 2. 10. |> rotate 20.) ]
  | Wand -> [ at 0. 0. (rectangle ink 4. 26. |> rotate (-45.)); at 9. 9. (circle ink 4.); at 13. 2. (rectangle ink 2. 2.); at 2. 13. (rectangle ink 2. 2.) ]
  | Pencil -> [ at 0. 0. (rectangle ink 5. 26. |> rotate (-45.)); at (-10.) (-10.) (rectangle ink 3. 3.) ]
  | Paintbrush -> [ at 4. 4. (rectangle ink 4. 20. |> rotate (-45.)); at (-7.) (-7.) (circle ink 5.) ]
  | Airbrush -> [ at 3. 3. (rectangle ink 8. 18. |> rotate (-45.)); at (-9.) (-8.) (circle ink 1.5); at (-12.) (-4.) (circle ink 1.5); at (-5.) (-12.) (circle ink 1.5) ]
  | Eraser -> [ at 0. 0. (rectangle ink 24. 13. |> rotate 30.); at 0. 0. (rectangle paper 20. 9. |> rotate 30.) ]
  | Stamp -> [ at 0. 6. (rectangle ink 8. 14.); at 0. (-4.) (rectangle ink 22. 6.); at 0. (-10.) (rectangle ink 24. 3.) ]
  | Smudge -> [ at 0. 4. (rectangle ink 6. 18. |> rotate (-20.)); at (-4.) (-9.) (oval ink 9. 7.) ]
  | Bucket -> [ at 0. 0. (rectangle ink 17. 17. |> rotate 30.); at 0. 0. (rectangle paper 13. 13. |> rotate 30.); at 11. (-8.) (rectangle ink 3. 8.) ]
  | Gradient_tool -> List.init 6 (fun i -> let g = 40 * i in at (-12.5 +. (float_of_int i *. 5.)) 0. (rectangle (rgb g g g) 5. 24.))
  | Eyedropper -> [ at 2. 2. (rectangle ink 4. 22. |> rotate (-45.)); at 8. 8. (rectangle ink 8. 8. |> rotate (-45.)); at (-9.) (-9.) (circle ink 2.) ]

let histogram_shapes (img : Rgba_image.t) : shape list =
  let h = Histogram.compute img in
  (* 64 bars of 4 values each, the tallest 80 high *)
  let bars = Array.init 64 (fun i -> h.luminance.(4 * i) + h.luminance.((4 * i) + 1) + h.luminance.((4 * i) + 2) + h.luminance.((4 * i) + 3)) in
  let top = Array.fold_left max 1 bars in
  (rectangle white 200. 90. |> move panel_x 235.)
  :: List.init 64 (fun i ->
         let hgt = 80. *. float_of_int bars.(i) /. float_of_int top in
         rectangle black 3. (Float.max 0.5 hgt) |> move (panel_x -. 100. +. 1.5 +. (float_of_int i *. 3.125)) (195. +. (hgt /. 2.)))

let dialog_view (m : model) (d : dialog) : shape list =
  let th = Gui.theme () in
  let panel = [ rectangle black (panel_w +. 4.) 520. |> move panel_x 60.; rectangle th.face panel_w 516. |> move panel_x 60.; words black d.title |> move panel_x 305. ] in
  let hist = if d.histogram then histogram_shapes (picture m) else [] in
  let labels =
    List.concat
      (List.mapi
         (fun i ((label, _, _, show), v) ->
           let b = slider_box i in
           [ words black (label ^ ": " ^ show v) |> move panel_x (b.y +. 26.) ])
         (List.combine d.sliders d.values))
  in
  (* a colour picker shows its colour *)
  let swatch = if d.preview = None && List.length d.values = 3 && d.title <> "Feather" then match ints d.values with [ r; g; b ] -> [ rectangle (colour (r, g, b)) 120. 40. |> move panel_x (-70.) ] | _ -> [] else [] in
  panel @ hist @ labels @ swatch

let palette_view (m : model) : shape list =
  let th = Gui.theme () in
  let d = doc m in
  let n = List.length d.layers in
  let panel = [ rectangle black (panel_w +. 4.) 520. |> move panel_x 60.; rectangle th.face panel_w 516. |> move panel_x 60.; words black "Layers" |> move panel_x 305. ] in
  let labels = [ words black "Mode" |> move panel_x (mode_box.y +. 24.); words black (Printf.sprintf "Opacity: %d%%" (int_of_float (Float.round ((layer m).opacity *. 100.)))) |> move panel_x (opacity_box.y +. 24.) ] in
  let row i =
    let index = n - 1 - i in
    let l = List.nth d.layers index in
    let b = row_box i and e = eye_box i in
    let shade = if index = d.current then rgb 170 190 225 else white in
    [ rectangle black (b.w +. 2.) (b.h +. 2.) |> move b.x b.y; rectangle shade b.w b.h |> move b.x b.y ]
    @ (if l.visible then [ oval black 16. 9. |> move e.x e.y; circle shade 2.5 |> move e.x e.y ] else [ rectangle (rgb 150 150 150) 14. 1. |> move e.x e.y ])
    @ [ bitmap 24. 24. l.image |> move (b.x -. 62.) b.y; words black l.name |> move (b.x +. 30.) b.y ]
  in
  panel @ labels @ List.concat (List.init (min n rows) row)

let view (computer : computer) (m : model) : shape list =
  let th = Gui.theme () in
  let img = match m.dialog with Some { cache = Some (_, preview); _ } -> flatten preview | _ -> composite m in
  let s = scale img and left, top = origin img in
  let w = float_of_int img.width *. s and h = float_of_int img.height *. s in
  let window =
    [
      rectangle black (w +. 4.) (h +. 30.) |> move (left +. (w /. 2.)) (top -. (h /. 2.) +. 13.);
      rectangle white w 24. |> move (left +. (w /. 2.)) (top +. 13.);
      words black (m.name ^ if s < 1. then Printf.sprintf " (%d%%)" (int_of_float (s *. 100.)) else "") |> move (left +. (w /. 2.)) (top +. 13.);
    ]
  in
  let picture_shapes =
    List.map
      (fun ((x, y, tw, th), t) -> bitmap (float_of_int tw *. s) (float_of_int th *. s) t |> move (left +. ((float_of_int x +. (float_of_int tw /. 2.)) *. s)) (top -. ((float_of_int y +. (float_of_int th /. 2.)) *. s)))
      (tiles img)
  in
  let selection = match m.selection with Some sel -> ants computer img sel | None -> [] in
  let lasso =
    match m.drag with
    | Some (Lassoing (points, _)) ->
        List.map (fun (x, y) -> rectangle black 2. 2. |> move (left +. (x *. s)) (top -. (y *. s))) points
    | _ -> []
  in
  let palette =
    List.concat
      (List.mapi
         (fun i t ->
           let b = tool_box i in
           let ink, paper = if t = m.tool then (white, black) else (black, white) in
           [ rectangle paper b.w b.h |> move b.x b.y ] @ Gui.shapes (Widget.frame black 1. b) @ icon b t ~ink ~paper)
         tools)
  in
  let colours =
    [ rectangle black (bg_box.w +. 2.) (bg_box.h +. 2.) |> move bg_box.x bg_box.y; rectangle (colour m.bg) bg_box.w bg_box.h |> move bg_box.x bg_box.y;
      rectangle black (fg_box.w +. 2.) (fg_box.h +. 2.) |> move fg_box.x fg_box.y; rectangle (colour m.fg) fg_box.w fg_box.h |> move fg_box.x fg_box.y ]
  in
  let options =
    if m.dialog <> None then []
    else
      List.mapi
        (fun i (label, v) -> let b = option_box i in words black (label ^ v) |> move b.x (b.y +. 24.))
        [ ("Brush size: ", string_of_int (int_of_float m.radius)); ("Hardness: ", percent (m.hardness *. 100.)); ("Opacity: ", percent (m.opacity *. 100.));
          ("Tolerance: ", string_of_int (int_of_float m.tolerance)) ]
  in
  let info =
    let p = dot_at img (computer.mouse.mx, computer.mouse.my) in
    if on_picture img p then
      let r, g, b = pixel img p in
      Printf.sprintf "x %d  y %d   R %d  G %d  B %d" (int_of_float (fst p)) (int_of_float (snd p)) r g b
    else ""
  in
  let status =
    Printf.sprintf "%s     %s     %s     %s" (tool_name m.tool)
      (match Undo.undo_name m.history with Some n -> "Undo " ^ n | None -> "")
      info
      (if File_menu.said m.file <> "" then File_menu.said m.file else if m.tool = Stamp && m.source = None then "Alt-click where to copy from" else "")
  in
  [ rectangle (rgb 150 150 150) 1000. 1000.; rectangle th.face 1000. 40. |> move 0. 475. ]
  @ window @ picture_shapes @ selection @ lasso @ palette @ colours @ options
  @ (match m.dialog with Some d -> dialog_view m d | None -> palette_view m)
  @ [ words (rgb 30 30 30) status |> move 0. (-440.) ]
  @ File_menu.view m.file @ Gui.draw ()

let app caps = game view (update caps) (Lazy.force initial)
let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> File_menu.caps)))
