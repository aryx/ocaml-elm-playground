open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers modules *)
(*****************************************************************************)

let log s =
  Printf.printf "%s" s

module Basics = struct
let (/..) = (/)
let (+..) = (+)
let (-..) = (-)
let ( *.. ) = ( * )

let (/) = (/.)
let (+) = (+.)
let (-) = (-.)
let ( * ) = ( *. )

let (round: float -> int) = fun f ->
    int_of_float (floor (f +. 0.5))

let mod_by a b =
  b mod a

let pi = Float.pi
let pi2 = 8. *. atan 1.

(* was called just degrees in Basics.elm *)
let degrees_to_radians deg =
  (deg * pi) / 180.

let (turns: float -> float) = fun angle_in_turns ->
    2. * pi * angle_in_turns

let (clamp: 'number -> 'number -> 'number -> 'number) = fun low high number ->
  if number < low then
    low
  else if number > high then
    high
  else
    number
end
open Basics

module Set = struct
type 'a t = 'a list
let empty = []
end

module Time = struct
type posix = float
let millis_to_posix n =
  float_of_int n
let posix_to_millis n =
  int_of_float ( n * 1000.)
end



module Cmd = struct
type 'msg t = None | Msg of 'msg
let none = None
end

module Sub = struct
type 'msg t =  
  | SubTick of (Time.posix -> 'msg)
  | SubMouseMove of (float * float -> 'msg)

  | Batch of 'msg t list
let none = Batch []
let batch xs = Batch xs
end


module Html = struct
type 'msg t = UI of 'msg 
type 'msg vdom = VNone
end

module Platform = struct

type ('model, 'msg) app =
  {
    init: ('model * 'msg Cmd.t);
    update: ('model -> 'msg -> 'model * 'msg Cmd.t);
    view: ('model -> 'msg Html.vdom);
    subscriptions: ('model -> 'msg Sub.t);
  }

let app ~init ~update ~view ~subscriptions () =
  {init; update; view; subscriptions}

(* flags? *)
type ('flags, 'model, 'msg) program = 
    ('model, 'msg) app

end




module Window = struct
type 'msg document = {
  title: string;
  body: 'msg Html.vdom list;
}

type ('flags, 'model, 'msg) app = {
  init: 'flags -> ('model * 'msg Cmd.t);
  view: 'model -> 'msg document;
  update: 'msg -> 'model -> ('model * 'msg Cmd.t);
  subscriptions: 'model -> 'msg Sub.t;
}

let (document: ('flags, 'model, 'msg) app -> 
               ('flags, 'model, 'msg) Platform.program) 
 = fun { init; view; update; subscriptions } ->
  Platform.app 
      ~init:(init ()) 
      ~view:(fun model ->
        match view model with
        (* TODO: do a div? *)
        | {title=_; body} -> List.hd body
      )
      ~update:(fun model msg -> update msg model) 
      ~subscriptions:subscriptions
     ()

end

module Event = struct
type visibility = 
  | Visible

let (on_animation_frame: (Time.posix -> 'msg) -> 'msg Sub.t) = fun f ->
  Sub.SubTick f

let (on_mouse_move: (float * float -> 'msg) -> 'msg Sub.t) = fun f ->
  Sub.SubMouseMove f
end

(*****************************************************************************)
(* Real Start of Playground *)
(*****************************************************************************)
(* in Playground.elm *)

(* You should not depend on Vdom starting from here, but instead
 * rely on the helper modules above (which internally use Vdom).
 *)

(*****************************************************************************)
(* Number *)
(*****************************************************************************)

type number = float

let string_of_floatint x = 
  spf "%f" x

(*****************************************************************************)
(* Time (and animations) *)
(*****************************************************************************)

type time = Time of Time.posix

let (to_frac: float -> time -> float) = fun period (Time posix) ->
    let ms = Time.posix_to_millis posix in
    let p = period *. 1000. in
    if p = 0. || ms = 0
    then failwith "division by zero in to_frac";
    pr2_gen (ms, p);
    float (mod_by (round p) ms) / p

(* period is in seconds *)
let (spin: number -> time -> number) = fun period time ->
    360. * to_frac period time
    |> (fun x -> pr2_gen (period, time, x); x)

let (wave: number -> number -> number -> time -> number) = 
 fun lo hi period time ->
    lo + (hi - lo) * (1. + cos (turns (to_frac period time))) / 2.

let (zigzag: number -> number -> number -> time -> number) = 
 fun lo hi period time ->
    lo + (hi - lo) * abs_float (2. * to_frac period time - 1.)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

type color =
  | Hex of string
  | Rgb of int * int * int

let white = Hex "#FFFFFF"
let black = Hex "#000000"

let red   = Hex "#cc0000"
let green = Hex "#73d216"
let blue  = Hex "#3465a4"

let brown = Hex "#c17d11"

(*-------------------------------------------------------------------*)
(* Light colors *)
(*-------------------------------------------------------------------*)

let lightYellow = Hex "#fce94f"
let lightPurple = Hex "#ad7fa8"

(*-------------------------------------------------------------------*)
(* Dark colors *)
(*-------------------------------------------------------------------*)

(*-------------------------------------------------------------------*)
(* Shades of grey *)
(*-------------------------------------------------------------------*)

let darkGray = Hex "#babdb6"

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

type shape = {
    x: number; 
    y: number; 
    (* in degrees *)
    angle: number;
    scale: number;
    (* [0..1] range *)
    alpha: number;

    form: form
}
and form = 
  | Circle of color * number (* radius *)
  | Oval      of color * number * number
  | Rectangle of color * number * number
  | Ngon of color * int * number

(* less: could use deriving constructor? *)
let shape x y angle scale alpha form =
  { x; y; angle; scale; alpha; form }

(*-------------------------------------------------------------------*)
(* Shape constructors *)
(*-------------------------------------------------------------------*)

let (circle: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Circle (color, radius))

let (oval: color -> number -> number -> shape) = fun color width height ->
  shape 0. 0. 0. 1. 1. (Oval (color, width, height))

let (rectangle: color -> number -> number -> shape) = fun color width height ->
  shape 0. 0. 0. 1. 1. (Rectangle (color, width, height))

let (square: color -> number -> shape) = fun color n ->
  shape 0. 0. 0. 1. 1. (Rectangle (color, n, n))

let (triangle: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Ngon (color, 3, radius))

let (pentagon: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Ngon (color, 5, radius))

let (hexagon: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Ngon (color, 6, radius))

let (octagon: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Ngon (color, 8, radius))

(*-------------------------------------------------------------------*)
(* Move shapes *)
(*-------------------------------------------------------------------*)

let (move: number -> number -> shape -> shape) = 
  fun dx dy {x; y; angle; scale; alpha; form } ->
    {x = x + dx; y = y + dy; angle; scale; alpha; form}

let (move_left: number -> shape -> shape) = 
  fun dx {x; y; angle; scale; alpha; form } ->
    {x = x - dx; y; angle; scale; alpha; form}

let (move_down: number -> shape -> shape) = 
  fun dy {x; y; angle; scale; alpha; form } ->
    {x; y = y - dy; angle; scale; alpha; form}

let (move_x: number -> shape -> shape) = 
  fun dx {x; y; angle; scale; alpha; form } ->
    {x = x + dx; y; angle; scale; alpha; form}

let (move_y: number -> shape -> shape) = 
  fun dy {x; y; angle; scale; alpha; form } ->
    {x; y = dy + y; angle; scale; alpha; form}

let move_right = move_x
let move_up = move_y

(*-------------------------------------------------------------------*)
(* Customize shapes *)
(*-------------------------------------------------------------------*)

let (rotate: number -> shape -> shape) = 
  fun da {x; y; angle; scale; alpha; form } ->
    {x; y; angle = angle + da; scale; alpha; form}

let (fade: number -> shape -> shape) = 
  fun o {x; y; angle; scale; alpha = _; form } ->
    {x; y; angle; scale; alpha = o; form}
  

(*****************************************************************************)
(* Computer *)
(*****************************************************************************)

(*-------------------------------------------------------------------*)
(* Screen *)
(*-------------------------------------------------------------------*)

type screen = {
  width: number;
  height: number;
  top: number;
  left: number;
  right: number;
  bottom: number;
}

let (to_screen: number -> number -> screen) = fun width height ->
  { width; 
    height; 
    (* the origin (0, 0) is at the center of the screen *)
    top = height / 2.;
    left = (-. width) / 2.;
    right = width / 2.;
    bottom = (-. height) / 2.;
  }

(*-------------------------------------------------------------------*)
(* Mouse *)
(*-------------------------------------------------------------------*)
(* in screen-centered coordinate (0, 0) at center of screen *)
type mouse = {
  mx: number;
  my: number;

  mdown: bool;
  mclick: bool;
}

let mouse_move mx my mouse = 
  { mouse with mx; my }
let mouse_click mclick mouse =
  { mouse with mclick }
let mouse_down mdown mouse =
  { mouse with mdown }

(*-------------------------------------------------------------------*)
(* Keyboard *)
(*-------------------------------------------------------------------*)
type keyboard = {
  kup: bool;
  kdown: bool;
  kleft: bool;
  kright: bool;
  
  kspace: bool;
  kenter: bool;
  kshift: bool;
  kbackspace: bool;
  
  keys: string Set.t;  
}

let empty_keyboard = {
  kup = false; kdown = false; kleft = false; kright = false;
  kspace = false; kenter = false; kshift = false; kbackspace = false;
  keys = Set.empty
}

let to_x keyboard =
  (if keyboard.kright then 1. else 0.) - (if keyboard.kleft then 1. else 0.)

let to_y keyboard =
  (if keyboard.kup then 1. else 0.) - (if keyboard.kdown then 1. else 0.)

let square_root_two =
  sqrt 2.

let to_xy keyboard =
  let x = to_x keyboard in
  let y = to_y keyboard in
  if x <> 0. && y <> 0.
  then (x / square_root_two, y / square_root_two)
  else (x, y)

let string_of_intkey = function
  | _ -> "TODO"

let update_keyboard _is_down key keyboard =
  log key;
  keyboard

(*-------------------------------------------------------------------*)
(* Memory *)
(*-------------------------------------------------------------------*)

(*-------------------------------------------------------------------*)
(* Computer *)
(*-------------------------------------------------------------------*)
type computer = {
  mouse: mouse;
  keyboard: keyboard;
  screen: screen;
  time: time;
}

let initial_computer = {
  mouse = { mx = 0.; my = 0.; mdown = false; mclick = false };
  keyboard = empty_keyboard;
  screen = to_screen 600. 600.;
  time = Time (Time.millis_to_posix 1);
}

(*****************************************************************************)
(* Render *)
(*****************************************************************************)

module Cairo2 = struct
let (cr: Cairo.context option ref) = ref None
let get_cr () = 
  match !cr with
  | None -> failwith "no cr"
  | Some x -> x

let sx = ref 0
let sy = ref 0

let set_color cr color alpha =
  let (r,g,b) =
    match color with
    | Rgb (r,g,b) -> float r, float g, float b
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
  let sx = !sx in
  let sy = !sy in
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
end


(*    
let render_transform x y a s =
  if a = 0. then
    if s = 1.
    then
      spf "translate(%s, %s)" 
        (string_of_floatint x) (string_of_floatint (-. y))
    else
      spf "translate(%s, %s) scale(%s)" 
        (string_of_floatint x) (string_of_floatint (-. y))
        (string_of_floatint s)
 else
  if s = 1.
  then
      spf "translate(%s, %s) rotate(%s)" 
        (string_of_floatint x) (string_of_floatint (-. y))
        (string_of_floatint (-. a))
  else
      spf "translate(%s, %s) rotate(%s) scale(%s) " 
        (string_of_floatint x) (string_of_floatint (-. y))
        (string_of_floatint (-. a))
        (string_of_floatint s)

let render_rect_transform width height x y angle s =
  render_transform x y angle s ^
  spf " translate(%s, %s)" 
     (string_of_floatint (-. width / 2.))
     (string_of_floatint (-. height / 2.))



*)

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

let render_ngon cr n radius x y angle s = 
  (*pr2_gen (x,y,n,radius);*)
  let (x, y) = Cairo2.convert (x, y) in

  Cairo.save cr;

  Cairo.translate cr x y;
  Cairo.rotate cr (-. (Basics.degrees_to_radians angle));
  pr (spf "rotate: %.1f" angle);

  ngon_points cr 0 n radius;
  Cairo.fill cr;

  Cairo.restore cr;
  ()

let render_oval cr w h x y angle s = 
  (*pr2_gen (x,y,w,h);*)

  let x = x - (w / 2.) in
  let y = y + (h / 2.) in
  let (x,y) = Cairo2.convert (x,y) in

  (* code in cairo.mli to draw ellipsis *)
  Cairo.save cr;

  Cairo.translate cr (x +. w /. 2.) (y +. h /. 2.);
  Cairo.scale cr (w /. 2.) (h /. 2.);

  Cairo.arc cr 0. 0. 1. 0. pi2;
  Cairo.fill cr;

  Cairo.restore cr;
  ()

let render_rectangle cr w h x y angle s = 
  (*pr2_gen (x,y,w,h);*)
  let x0 = x - (w / 2.) in
  let y0 = y + (h / 2.) in
  let (x0,y0) = Cairo2.convert (x0,y0) in
  Cairo.save cr;

  Cairo.rotate cr (-. (Basics.degrees_to_radians angle));
  pr (spf "rotate: %.1f" angle);

  Cairo.rectangle cr x0 y0 w h;
  Cairo.fill cr;

  Cairo.restore cr;
  ()


let render_circle cr radius x y angle s =
  (*pr2_gen (x,y, radius);*)
  let (x,y) = Cairo2.convert (x,y) in

  Cairo.save cr;

  Cairo.arc cr x y radius 0. pi2;
  Cairo.fill cr;

  Cairo.restore cr;
  ()

let (render_shape: shape -> 'msg Html.vdom) = 
  fun { x; y; angle; scale; alpha; form} ->
  let cr = Cairo2.get_cr () in

  (match form with
  | Circle (color, radius) -> 
     Cairo2.set_color cr color alpha;
     render_circle cr radius x y angle scale
  | Oval (color, width, height) ->
     Cairo2.set_color cr color alpha;
     render_oval cr width height x y angle scale
  | Rectangle (color, width, height) ->
     Cairo2.set_color cr color alpha;
     render_rectangle cr width height x y angle scale
  | Ngon (color, n, radius) ->
     Cairo2.set_color cr color alpha;
     render_ngon cr n radius x y angle scale
  );
  Html.VNone


let (render: screen -> shape list -> 'msg Html.vdom) = fun screen shapes ->
    let w = screen.width  in
    let h = screen.height  in
    let _x = screen.left |> string_of_floatint  in
    let _y = screen.bottom |> string_of_floatint in

    let _vdoms = List.map render_shape shapes in
    Html.VNone

(*****************************************************************************)
(* Playground: picture *)
(*****************************************************************************)

type msg1 = 
  | Resized of int * int

let (picture: shape list -> (unit, screen, msg1) Platform.program) = 
 fun shapes ->
  let init () = 
      to_screen 600. 600., Cmd.none
  in
  let view screen = 
      { Window.
        title = "Playground";
        body = [ render screen shapes ]
      }
  in
  let update msg  _model = 
    match msg with
    | Resized (width, height) ->
       to_screen (float width) (float height), Cmd.none
  in
  let subscriptions _ =
      Sub.none
  in
  Window.document { Window. init; view; update; subscriptions }

(*****************************************************************************)
(* Playground: animation *)
(*****************************************************************************)

(* this is also used by the game playground *)
type msg =
  | Tick of Time.posix
  | Resized of int * int
  (* ... *)

  | KeyChanged of bool * string

  | MouseMove of (float * float)
  | MouseClick
  | MouseButton of bool (* true = down, false = up *)
  


type animation = Animation of Event.visibility * screen * time

let animation_update msg (Animation (v, s, t) as state) =
  match msg with
  | Tick posix -> 
    pr2 "HERE"; pr2_gen posix;
    Animation (v, s, (Time posix))
  | Resized (w, h) -> 
    Animation (v, to_screen (float w) (float h), t)
  | MouseMove _ 
  | MouseClick 
  | MouseButton _
  | KeyChanged _
    -> state

let (animation: (time -> shape list) -> (unit, animation, msg) Platform.program) =
 fun view_frame ->
   let init () = 
     Animation (Event.Visible, 
                to_screen 600. 600., 
                (* bugfix: use 1, not 0, otherwise get div_by_zero exn in
                 * to_frac if use spin/wave/... *)
                Time (Time.millis_to_posix 1)),
     Cmd.none
   in
   let view (Animation (_, screen, time)) =
     { Window.
       title = "Playground";
       body = [render screen (view_frame time)];
     }
   in
   let update msg model = 
     animation_update msg model,
     Cmd.none
   in
  let subscriptions _ =
      Event.on_animation_frame (fun x -> Tick x)
  in
  Window.document { Window. init; view; update; subscriptions }

(*****************************************************************************)
(* Playground: game *)
(*****************************************************************************)
type 'memory game = Game of Event.visibility * 'memory * computer

let (game_update: (computer -> 'memory -> 'memory) -> msg -> 'memory game ->
 'memory game) =
 fun _update_memory msg (Game (vis, memory, computer)) ->
    match msg with
    | Tick _time ->
        raise Todo
    | Resized (_w, _h) ->
        raise Todo
    | MouseMove (x, y) ->
(*
        let x = computer.screen.left + page_x in
        let y = computer.screen.top - page_y in
*)
        Game (vis, memory, 
             { computer with mouse = mouse_move x y computer.mouse })
    | MouseClick ->
        Game (vis, memory, 
             { computer with mouse = 
                (* Vdom does not provide OnMouseUp, so we use MouseClick *)
                 mouse_down false 
                   (mouse_click true computer.mouse) })
    | MouseButton is_down ->
        Game (vis, memory, 
             { computer with mouse = mouse_down is_down computer.mouse })
    | KeyChanged (is_down, key) ->
        Game (vis, memory,
             { computer with keyboard = update_keyboard is_down key 
                 computer.keyboard })
        

let (game: 
  (computer -> 'memory -> shape list) ->
  (computer -> 'memory -> 'memory) ->
  'memory ->
  (unit, 'memory game, msg) Platform.program) = 
 fun view_memory update_memory initial_memory ->

  let init () =
      Game (Event.Visible, initial_memory, initial_computer),
      Cmd.none (* TODO: Task.perform GotViewport Dom.getViewport *)
  in
  let view (Game (_, memory, computer)) =
    let elt = render computer.screen (view_memory computer memory) in

(*
      (* TODO: should use subscription instead *)
    let elt = Html.div [
        V.onmousemove (fun evt -> 
          MouseMove (evt.V.page_x, evt.V.page_y)
        );
        V.onclick (fun _evt -> 
          log "click"; 
          MouseClick
        );
        (* note that Vdom does not provide onmouseup, so we use onclick 
         * to set back mouse.mdown to false *)
        V.onmousedown (fun evt -> 
          log (spf "%d" evt.V.buttons);
          MouseButton (evt.V.buttons > 0)
        );
        V.onkeydown (fun evt -> 
          log (spf "key: %d" evt.V.which);
          KeyChanged (true, string_of_intkey evt.V.which)
        );
      ] [elt] 
    in
*)

    { Window.
      title = "Playground";
      body = [elt];
    }
  in
  let update msg model =
      game_update update_memory msg model,
      Cmd.none
  in
  let subscriptions _ =
      Event.on_mouse_move (fun x -> MouseMove x)
  in
  Window.document { Window. init; view; update; subscriptions }

(*****************************************************************************)
(* run_app *)
(*****************************************************************************)

let pi2 = 8. *. atan 1.

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


let run_app app =
  Graphics.open_graph ":0 600x600+0+0";
  (* TODO: WEIRD: resize_window does not seem to work; the command
   * do not display on the screen after that, hence the manual geometry
   * before in the call to open_graph.
   *
   * Graphics.resize_window 600 600;
   *)
  Graphics.clear_graph ();
  let sx = Graphics.size_x () in
  let sy = Graphics.size_y () in
  pr2_gen (sx, sy);
  Graphics.auto_synchronize false;

  let cr_img = Cairo.Image.create Cairo.Image.RGB24 sx sy in
  let cr = Cairo.create cr_img in

  Cairo.identity_matrix cr;
  Cairo2.cr := Some cr;
  Cairo2.sx := sx;
  Cairo2.sy := sy;
  Cairo2.debug_coordinates cr;

  let initmodel, _cmdsTODO = app.Platform.init in
  let model = ref initmodel in

  let status = ref (Graphics.wait_next_event [Graphics.Poll]) in

  while true do
    Cairo.save cr;

    (* reset the surface content *)
    (* do not use 1. 1. 1. (pure white) because Graphics seems to
     * consider that a transparent color and it does not clear the
     * screen.
     *)
    Cairo.set_source_rgba cr 0.99 0.99 0.99 1.;
    Cairo.paint cr;

    (* elm-convetion: set the origin (0, 0) in the center of the surface *)
    Cairo.identity_matrix cr;
    Cairo.translate cr (float sx / 2.) (float sy / 2.);
    Cairo2.debug_coordinates cr;

    (* one frame *)
    let time = Unix.gettimeofday() in

    let sub = app.Platform.subscriptions !model in
    let new_status = 
      Graphics.wait_next_event [
        Graphics.Button_down;
        Graphics.Button_up;
        Graphics.Key_pressed;
        Graphics.Mouse_motion;
        Graphics.Poll
        ]
    in
    let msg_opt = 
      match sub with
      | Sub.Batch [] -> None
      | Sub.Batch xs -> 
          (* select on event list *)
          raise Todo

      | Sub.SubTick f ->
         Some (f time)

      | Sub.SubMouseMove f ->
         let x, y = Graphics.mouse_pos () in
         let (x, y) = Cairo.device_to_user cr (float x) (float y) in
        (* note that Graphics origin is at the bottom left, not
         * top left like in Cairo, which is why we don't need 
         * to call convert here 
         * let (x, y) = Cairo2.convert (x, y) in
         *)
         pr2_gen (x, y);
         Some (f (x, y))
    in
    (match msg_opt with
    | None -> ()
    | Some msg ->
      let newmodel, _cmds = app.Platform.update !model msg in
      model := newmodel;
    );

    let _vdom = app.Platform.view !model in

    Cairo.restore cr;
    draw_fps cr (float sx) (float sy);

    (* Don't forget to flush the surface before using its content. *)
    Cairo.Surface.flush cr_img;
    (* Now, access the surface data and convert it to a Graphics.image
       that can be drawn on the Graphics window. *)
    let data32 = Cairo.Image.get_data32 cr_img in
    let data_img =
      Array.init sy
        (fun y -> Array.init sx (fun x -> Int32.to_int (data32.{y, x})))
    in
    Graphics.draw_image (Graphics.make_image data_img) 0 0;
    Graphics.synchronize ();
    (* Update our fps counter. *)
    update_fps ()
  done
