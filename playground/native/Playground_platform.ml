open Common
open Basics
open Color

open Tsdl

(*****************************************************************************)
(* Render (independent of Playground) *)
(*****************************************************************************)

let (g_cr: Cairo.context option ref) = ref None
let get_cr () = 
  match !g_cr with
  | None -> failwith "no cr"
  | Some x -> x

let g_sx = ref 0
let g_sy = ref 0

(* less: save_excursion? *)
let with_cr f = 
  let cr = get_cr () in
  Cairo.save cr;
  let res = f cr in
  Cairo.restore cr;
  res

let set_color cr color alpha =
  let (r,g,b) =
    match color with
    | Rgb (r,g,b) -> float r / 255., float g / 255., float b / 255.
    | Hex s ->
        let s = String.lowercase_ascii s in
        if s =~ "^#\\([a-f0-9][a-f0-9]\\)\\([a-f0-9][a-f0-9]\\)\\([a-f0-9][a-f0-9]\\)$"
        then 
          let (a, b, c) = Common.matched3 s in
          let f x =
            (("0x" ^ x) |> int_of_string |> float) / 255.
          in
          f a, f b, f c
        else failwith (spf "wrong color format: %s" s)
  in
  Cairo.set_source_rgba cr r g b (clamp 0. 1. alpha)

let debug_coordinates cr = 
  let sx = !g_sx in
  let sy = !g_sy in
  let (x0,y0) = Cairo.device_to_user cr 0. 0. in
  let (xmax, ymax) = Cairo.device_to_user cr (float sx) (float sy) in
  pr2 (spf "device 0,0 => %.1f %.1f, device %d,%d => %.1f %.1f"
    x0 y0 sx sy xmax ymax)

(* Cairo (0,0) is at the top left of the screen, in which as y goes up,
 * the coordinates are down on the physical screen. Elm uses a better
 * default where if your y goes up, then it's upper on the screen.
 * Here we convert the Elm coordinate system to Cairo. Note that
 * it's hard to use one of the rotate/translate Cairo function to emulate
 * that as only y need to change (maybe need to create a special matrix?)
 *)
let convert (x, y) =
  x, -. y


let render_transform cr x y angle s =
  Cairo.translate cr x y;
  Cairo.rotate cr (-. (Basics.degrees_to_radians angle));
  Cairo.scale cr s s;
  ()


let rec ngon_points cr i n radius =
  if i == n 
  then ()
  else begin
    let a = turns (float i / float n - 0.25) in
    let x = radius * cos a in
    let y = radius * sin a in
    (if i = 0
    then Cairo.move_to cr x y
    else Cairo.line_to cr x y
    );
    ngon_points cr (Stdlib.(+) i 1) n radius
  end

let render_ngon color n radius x y angle s alpha = 
  (*pr2_gen (x,y,n,radius);*)
  let (x, y) = convert (x, y) in

  with_cr (fun cr ->
    set_color cr color alpha;
    render_transform cr x y angle s;

    ngon_points cr 0 n radius;
    Cairo.fill cr;
  )


let render_oval color w h x y _angle _s alpha = 
  (*pr2_gen (x,y,w,h);*)

  let x = x - (w / 2.) in
  let y = y + (h / 2.) in
  let (x,y) = convert (x,y) in

  with_cr (fun cr -> 
    set_color cr color alpha;

    (* code in cairo.mli to draw ellipsis *)
    Cairo.translate cr (x +. w /. 2.) (y +. h /. 2.);
    Cairo.scale cr (w /. 2.) (h /. 2.);

    Cairo.arc cr 0. 0. 1. 0. pi2;
    Cairo.fill cr;
  )


let render_circle color radius x y angle s alpha =
  (*pr2_gen (x,y, radius);*)
  let (x,y) = convert (x,y) in

  with_cr (fun cr ->
    set_color cr color alpha;
    render_transform cr x y angle s;

    Cairo.arc cr 0. 0. radius 0. pi2;
    Cairo.fill cr;
  )

let render_polygon color points x y angle s alpha =
  let (x,y) = convert (x,y) in

  with_cr (fun cr ->
    set_color cr color alpha;
    render_transform cr x y angle s;

    (match points with
    | [] -> failwith "not enough points in polygon"
    | (x, y)::xs ->
       let (x,y) = convert (x,y) in
       Cairo.move_to cr x y;
       xs |> List.iter (fun (x, y) ->
         let (x,y) = convert (x,y) in
         Cairo.line_to cr x y
       );
       Cairo.line_to cr x y;
       Cairo.fill cr;
    )
  )


let render_rectangle color w h x y angle s alpha = 
  render_polygon color 
    [ (-. w / 2., h /. 2.);
      (   w / 2., h /. 2.);
      (   w / 2., -.h /. 2.);
      (-. w / 2., -.h /. 2.);
    ] x y angle s alpha

let render_words color str x y angle s alpha =
  let (x,y) = convert (x,y) in

  with_cr (fun cr ->
    set_color cr color alpha;
    render_transform cr x y angle s;

    let extent = Cairo.text_extents cr str in
    let tw = extent.Cairo.width in
    let th = extent.Cairo.height in

    Cairo.move_to cr (-. tw / 2.) (th / 2.);
    Cairo.show_text cr str;
  )

(*****************************************************************************)
(* Render playground *)
(*****************************************************************************)
open Playground

let (render_shape: shape -> unit) = 
  fun { x; y; angle; scale; alpha; form} ->
  match form with
  | Circle (color, radius) -> 
     render_circle color radius x y angle scale alpha
  | Oval (color, width, height) ->
     render_oval color width height x y angle scale alpha
  | Rectangle (color, width, height) ->
     render_rectangle color width height x y angle scale alpha
  | Ngon (color, n, radius) ->
     render_ngon color n radius x y angle scale alpha
  | Polygon (color, points) -> 
     render_polygon color points x y angle scale alpha
  | Words (color, str) ->
     render_words color str x y angle scale alpha
  
let (render: shape list -> unit) = fun shapes ->
    List.iter render_shape shapes

(*****************************************************************************)
(* Event to msg *)
(*****************************************************************************)

module E = Sub
(* TODO: move cairo stuff out and generalize? *)
let event_to_msgopt event subs cr =
  match event with
  | E.ETick time ->
      subs |> E.find_map_opt (function 
       | Sub.SubTick f -> Some (f time) 
       | _ -> None
      )
  | E.EMouseMove (x, y) ->
      subs |> E.find_map_opt (function 
        | Sub.SubMouseMove f ->

          (* TODO !!!cairo specific stuff!!! *)
          let (x, y) = Cairo.device_to_user cr (float x) (float y) in
          let (x, y) = convert (x, y) in
          pr2_gen (x, y);

          Some (f (x, y))

         | _ -> None
      )
  | E.EMouseButton (true) ->
      subs |> E.find_map_opt (function 
        | Sub.SubMouseDown f ->
           Some (f ())
       | _ -> None
      )
  | E.EMouseButton (false) ->
      subs |> E.find_map_opt (function 
        | Sub.SubMouseUp f ->
           Some (f ())
       | _ -> None
      )
  | E.EKeyChanged (true, key) ->
      subs |> E.find_map_opt (function 
        | Sub.SubKeyDown f ->
          pr2_gen key;

           Some (f key)
       | _ -> None
      )
  | E.EKeyChanged (false, key) ->
      subs |> E.find_map_opt (function 
        | Sub.SubKeyUp f ->
           Some (f key)
       | _ -> None
      )

(*****************************************************************************)
(* FPS (using Cairo) *)
(*****************************************************************************)

module Fps = struct
(* was in cairo/examples/graphics_demo.ml *)
let lastfps = ref (Unix.gettimeofday ())
let frames = ref 0
let fps = ref 0.

let update_fps () =
  let t = Unix.gettimeofday () in
  let dt = t -. !lastfps in
  if dt > 0.5 then (
    fps := float !frames /. dt;
    frames := 0;
    lastfps := t
  );
  incr frames

let draw_fps cr width height =
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.move_to cr (0.05 *. width) (0.95 *. height);
  Cairo.show_text cr (Printf.sprintf "%gx%g -- %.0f fps" width height !fps)
end

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

(* The tsdl library is a heavy user of Result, which is annoying
 * to check at every calls; fortunately OCaml 4.08 allow to define
 * monadic operators to remove some boilerplate!
 *)
let (let*) o f =
  match o with
  | Error (`Msg msg) ->
      failwith (spf "TSDL error: %s" msg)
  | Ok x -> f x

let scancode_to_keystring = function
 | "Left" -> "ArrowLeft"
 | "Right" -> "ArrowRight"
 | "Up" -> "ArrowUp"
 | "Down" -> "ArrowDown"

 | "Q" -> exit 0
 | s -> String.lowercase_ascii s

let run_app app =
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let* () = Sdl.init Sdl.Init.(video + events) in
  let* sdl_window = Sdl.create_window ~w:sx ~h:sy "Playground using SDL+Cairo"
    Sdl.Window.shown in
  let event = Sdl.Event.create () in

  let* window_surface = Sdl.get_window_surface sdl_window in

  let pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim pixels = sx *.. sy);

  (* less? need that? *)
  Bigarray.Array1.fill pixels 0xFFFFFFFFl ;
  let pixels =
    try
      let genarray = Bigarray.genarray_of_array1 pixels in
      Bigarray.reshape_2 genarray sy sx
    with _ ->
      let len = Bigarray.Array1.dim pixels in
      failwith (spf
        "Error while reshaping pixel array of length %d to screen size %d x %d"
        len sx sy)
  in
  (* Create a Cairo surface to write on the pixels *)
  let sdl_surface =
    Cairo.Image.create_for_data32 ~w:sx ~h:sy pixels
  in
  let cr = Cairo.create sdl_surface in

  Cairo.identity_matrix cr;
  g_cr := Some cr;
  g_sx := sx;
  g_sy := sy;
  debug_coordinates cr;

  let initmodel, _cmdsTODO = app.Playground.init () in
  let model = ref initmodel in

  (* typing "Q" will cause an 'exit 0' that will exit the loop *)
  while true do
    Cairo.save cr;

    (* reset the surface content *)
    Cairo.set_source_rgba cr 1. 1. 1. 1.;
    Cairo.paint cr;

    (* elm-convetion: set the origin (0, 0) in the center of the surface *)
    Cairo.identity_matrix cr;
    Cairo.translate cr (float sx / 2.) (float sy / 2.);
    (*debug_coordinates cr;*)

    (* one frame *)
    let time = Unix.gettimeofday() in

    let subs = app.Playground.subscriptions !model in

    let event = 
      if Sdl.poll_event (Some event)
      then
        let event_type = Sdl.Event.get event Sdl.Event.typ in
        (match event_type with
        | x when x = Sdl.Event.mouse_motion ->
          let mx = Sdl.Event.(get event mouse_motion_x) in
          let my = Sdl.Event.(get event mouse_motion_y) in
          E.EMouseMove (mx, my)

        | x when x = Sdl.Event.mouse_button_down ->
          E.EMouseButton (true)

        | x when x = Sdl.Event.mouse_button_up ->
          E.EMouseButton (false)

        | x when x = Sdl.Event.key_down -> 
          let key = Sdl.(get_key_name Event.(get event keyboard_keycode)) in
          let str = scancode_to_keystring key in
          E.EKeyChanged (true, str)

        | x when x = Sdl.Event.key_up -> 
          let key = Sdl.(get_key_name Event.(get event keyboard_keycode)) in
          let str = scancode_to_keystring key in
          E.EKeyChanged (false, str)

        (* default case *)
        | _ -> E.ETick time 
        )
      else E.ETick time 
    in

    let msg_opt = event_to_msgopt event subs cr in
    (match msg_opt with
    | None -> ()
    | Some msg ->
      let newmodel, _cmds = app.Playground.update msg !model in
      model := newmodel;
    );

    let shapes = app.Playground.view !model in
    render shapes;

    Cairo.restore cr;
    Fps.draw_fps cr (float sx) (float sy);

    (* Don't forget to flush the surface before using its content. *)
    Cairo.Surface.flush sdl_surface;
    let* () = Sdl.update_window_surface sdl_window in

    (* Update our fps counter. *)
    Fps.update_fps ();
  done
