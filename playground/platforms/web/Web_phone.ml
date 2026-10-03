(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Web_phone.mli *)

module E = Sub
open Js_browser

(*****************************************************************************)
(* Phones *)
(*****************************************************************************)
(* claude: a phone or a tablet (plan_mobile.md, notes_mobile.md), kept
 * apart from the desktop's simpler path: run_app calls these two, and
 * nothing else of it knows about fingers. *)

(* The page laid out at the phone's own width, not 980 pixels shrunk,
 * and no double-tap zoom: the viewport tag, added here so that no page
 * needs it; a drag the program's, not the page's scroll. *)
let phone_page () : unit =
  let doc = Ojs.get_prop_ascii Ojs.global "document" in
  let meta = Ojs.call doc "createElement" [| Ojs.string_to_js "meta" |] in
  Ojs.set_prop_ascii meta "name" (Ojs.string_to_js "viewport");
  Ojs.set_prop_ascii meta "content" (Ojs.string_to_js "width=device-width, initial-scale=1");
  ignore (Ojs.call (Ojs.get_prop_ascii doc "head") "appendChild" [| meta |]);
  List.iter
    (fun el ->
      let style = Ojs.get_prop_ascii el "style" in
      Ojs.set_prop_ascii style "touchAction" (Ojs.string_to_js "none");
      Ojs.set_prop_ascii style "userSelect" (Ojs.string_to_js "none"))
    [ Ojs.get_prop_ascii doc "documentElement"; Ojs.get_prop_ascii doc "body" ]

(* the page's document, an element made, its style set *)
let document_js () = Ojs.get_prop_ascii Ojs.global "document"
let create (tag : string) : Ojs.t = Ojs.call (document_js ()) "createElement" [| Ojs.string_to_js tag |]

let css (el : Ojs.t) (props : (string * string) list) : unit =
  let style = Ojs.get_prop_ascii el "style" in
  List.iter (fun (k, v) -> Ojs.set_prop_ascii style k (Ojs.string_to_js v)) props

let on (el : Ojs.t) (kind : string) (f : Ojs.t -> unit) : unit =
  ignore (Ojs.call el "addEventListener" [| Ojs.string_to_js kind; Ojs.fun_to_js 1 f |])

(* a character by its code, made by the browser: a string from here
 * would reach it as bytes (U+2328 as three characters) *)
let char_js (code : int) : Ojs.t = Ojs.call (Ojs.get_prop_ascii Ojs.global "String") "fromCharCode" [| Ojs.int_to_js code |]

(* The phone's own keyboard, for a program that wants letters: a text
 * field that cannot be seen, focused (the keyboard comes up) by the key
 * row's "abc"; what the keyboard types becomes keys, each down then up a
 * moment later (a game looks for a key held, or pressed this frame), and
 * the typed text. Returns: open it, or close it.
 *
 * The catches. A phone's keyboard often gives no key's name
 * ("Unidentified") and the character only in the field's input event:
 * the keys are made from that. One that does give a name (Enter,
 * Backspace, iOS's letters) goes the desktop's way, run_app's keydown,
 * and is not made twice. And Backspace in an empty field gives nothing:
 * the field keeps one character, for Backspace to take. *)
let phone_text_field ~(process : E.event -> unit) (parent : Ojs.t) : bool -> unit =
  let field = create "input" in
  List.iter (fun (k, v) -> ignore (Ojs.call field "setAttribute" [| Ojs.string_to_js k; Ojs.string_to_js v |]))
    [ ("autocomplete", "off"); ("autocorrect", "off"); ("autocapitalize", "off"); ("spellcheck", "false") ];
  (* seen by the phone, not by the person; 16px, or iOS zooms on focus *)
  css field [ ("position", "fixed"); ("left", "0"); ("bottom", "0"); ("width", "1px"); ("height", "1px");
              ("opacity", "0"); ("fontSize", "16px"); ("border", "0"); ("padding", "0") ];
  ignore (Ojs.call parent "appendChild" [| field |]);
  let reset () = Ojs.set_prop_ascii field "value" (Ojs.string_to_js "_") in
  reset ();
  let press (key : string) (typed : string option) =
    process (E.EKeyChanged (true, key));
    Option.iter (fun s -> process (E.ETyped s)) typed;
    ignore (Ojs.call Ojs.global "setTimeout" [| Ojs.fun_to_js 1 (fun _ -> process (E.EKeyChanged (false, key))); Ojs.int_to_js 80 |])
  in
  (* did the last keydown in the field name its key (then run_app's
   * keydown did it)? *)
  let named = ref false in
  on field "keydown" (fun e -> named := Ojs.string_of_js (Ojs.get_prop_ascii e "key") <> "Unidentified");
  on field "input" (fun e ->
      if not !named then begin
        let kind = Ojs.string_of_js (Ojs.get_prop_ascii e "inputType") in
        let data = Ojs.get_prop_ascii e "data" in
        if kind = "deleteContentBackward" then press "Backspace" None
        else if not (Ojs.is_null data) then
          String.iter (fun c -> let s = String.make 1 c in press (if c = ' ' then "space" else s) (Some s)) (Ojs.string_of_js data)
      end;
      named := false;
      reset ());
  fun opened -> if opened then (reset (); ignore (Ojs.call field "focus" [||])) else ignore (Ojs.call field "blur" [||])

(* The key row: the keys a game wants, a finger on each -- the arrows,
 * Space, Enter, Escape -- and "abc", the phone's own keyboard for
 * letters. A key is down while the finger is on it and up when it
 * lifts (or slides off): an arrow held moves, two fingers two keys. Its
 * default prevented, a tap on a key does not take the focus (the phone's
 * keyboard would close). *)
let row_height = 52

let key_row ~(process : E.event -> unit) ~(letters : bool -> unit) (parent : Ojs.t) : Ojs.t =
  let row = create "div" in
  css row [ ("position", "fixed"); ("left", "0"); ("bottom", "0"); ("right", "60px"); ("height", string_of_int row_height ^ "px");
            ("display", "none"); ("gap", "4px"); ("padding", "4px"); ("boxSizing", "border-box");
            ("background", "#333") ];
  let key ~(label : Ojs.t) ~(down : unit -> unit) ~(up : unit -> unit) =
    let k = create "div" in
    Ojs.set_prop_ascii k "textContent" label;
    css k [ ("flex", "1"); ("display", "flex"); ("alignItems", "center"); ("justifyContent", "center");
            ("fontSize", "18px"); ("borderRadius", "6px"); ("background", "rgba(255,255,255,0.8)"); ("color", "#222");
            ("fontFamily", "sans-serif") ];
    let held = ref false in
    let release _ = if !held then (held := false; up ()) in
    on k "pointerdown" (fun e -> ignore (Ojs.call e "preventDefault" [||]); held := true; down ());
    on k "mousedown" (fun e -> ignore (Ojs.call e "preventDefault" [||]));
    List.iter (fun kind -> on k kind release) [ "pointerup"; "pointercancel"; "pointerleave" ];
    ignore (Ojs.call row "appendChild" [| k |])
  in
  let named label name ?typed () =
    key ~label
      ~down:(fun () -> process (E.EKeyChanged (true, name)); Option.iter (fun s -> process (E.ETyped s)) typed)
      ~up:(fun () -> process (E.EKeyChanged (false, name)))
  in
  named (char_js 0x25C0) "ArrowLeft" ();
  named (char_js 0x25B2) "ArrowUp" ();
  named (char_js 0x25BC) "ArrowDown" ();
  named (char_js 0x25B6) "ArrowRight" ();
  named (Ojs.string_to_js "Space") "space" ~typed:" " ();
  named (Ojs.string_to_js "Enter") "Enter" ();
  named (Ojs.string_to_js "Esc") "Escape" ();
  (* "abc": the phone's keyboard, open or closed, a tap each *)
  let letters_open = ref false in
  key ~label:(Ojs.string_to_js "abc") ~down:(fun () -> letters_open := not !letters_open; letters !letters_open) ~up:(fun () -> ());
  ignore (Ojs.call parent "appendChild" [| row |]);
  row

(* The drawing on a phone: at the top of the screen, not in its middle
 * (an upright phone's square game had a band above it, and a keyboard
 * came up over its bottom half), and as high as the part of the screen
 * left: the visual viewport's, which an iPhone shrinks for its keyboard
 * while its page keeps its height (Android shrinks the page itself),
 * less the key row's when it is shown. Returns: set that reserve. *)
let phone_drawing (svg : Element.t) : int -> unit =
  let svg = Element.t_to_js svg in
  ignore (Ojs.call svg "setAttribute" [| Ojs.string_to_js "preserveAspectRatio"; Ojs.string_to_js "xMidYMin meet" |]);
  let vv = Ojs.get_prop_ascii Ojs.global "visualViewport" in
  let reserve = ref 0 in
  let fit () =
    let h = if Ojs.is_null vv then Ojs.float_of_js (Ojs.get_prop_ascii Ojs.global "innerHeight") else Ojs.float_of_js (Ojs.get_prop_ascii vv "height") in
    css svg [ ("height", Printf.sprintf "%.0fpx" (h -. float_of_int !reserve)) ]
  in
  fit ();
  if not (Ojs.is_null vv) then on vv "resize" (fun _ -> fit ());
  fun px -> reserve := px; fit ()

(* The phone's keys, all in one element over the program: the key row,
 * shown and hidden by a button in the corner (a keyboard's sign), and the
 * text field behind the row's "abc". Made at the first touch, so never
 * with a mouse alone; and then the drawing laid out for a phone.
 * Returns that element, whose taps are its own, not the program's. *)
let phone_keys ~(process : E.event -> unit) ~(svg : unit -> Element.t option) : unit -> Ojs.t =
  let made = ref None in
  fun () ->
    match !made with
    | Some keys -> keys
    | None ->
        let keys = create "div" in
        css keys [ ("position", "fixed"); ("left", "0"); ("right", "0"); ("bottom", "0"); ("zIndex", "10") ];
        ignore (Ojs.call (Ojs.get_prop_ascii (document_js ()) "body") "appendChild" [| keys |]);
        let letters = phone_text_field ~process keys in
        let row = key_row ~process ~letters keys in
        let reserve = match svg () with Some s -> phone_drawing s | None -> fun _ -> () in
        let button = create "div" in
        Ojs.set_prop_ascii button "textContent" (char_js 0x2328);
        css button [ ("position", "fixed"); ("right", "8px"); ("bottom", "4px"); ("width", "44px"); ("height", "44px");
                     ("lineHeight", "44px"); ("textAlign", "center"); ("fontSize", "26px"); ("borderRadius", "8px");
                     ("background", "rgba(255,255,255,0.75)"); ("color", "#222") ];
        ignore (Ojs.call keys "appendChild" [| button |]);
        on button "pointerdown" (fun e -> ignore (Ojs.call e "preventDefault" [||]));
        on button "mousedown" (fun e -> ignore (Ojs.call e "preventDefault" [||]));
        let shown = ref false in
        on button "click" (fun _ ->
            shown := not !shown;
            css row [ ("display", if !shown then "flex" else "none") ];
            reserve (if !shown then row_height else 0);
            if not !shown then letters false);
        made := Some keys;
        keys

(* A finger or a pen as the mouse, from pointer events, which every
 * browser sends -- Safari on iOS sends no mouse event for a tap on a
 * page listening on the window, and no browser one for a drag. The
 * mouse itself keeps its own events (run_app's). A tap is the button
 * down and up at the finger, a drag its moves between; two taps close
 * in time and place a double click. preventDefault: no mouse events of
 * the browser's after it, which would click twice. [svg]: the drawing,
 * once there is one (for adjust_x_y); [process]: run_app's; [keys]:
 * phone_keys', whose taps are its own. *)
let listen_to_fingers ~(svg : unit -> Element.t option) ~(process : E.event -> unit) ~(keys : unit -> Ojs.t) : unit =
  let last_tap = ref (-1000., 0., 0.) in
  let on_pointer evt =
    let get p = Ojs.get_prop_ascii (Event.t_to_js evt) p in
    if Ojs.string_of_js (get "pointerType") <> "mouse" && not (Ojs.bool_of_js (Ojs.call (keys ()) "contains" [| get "target" |])) then begin
      Event.prevent_default evt;
      Web_audio.resume_audio ();
      let cx = Ojs.float_of_js (get "clientX") and cy = Ojs.float_of_js (get "clientY") in
      let move () =
        match svg () with
        | Some svg ->
            let x, y = Web_events.adjust_x_y svg cx cy in
            process (E.EMouseMove (int_of_float x, int_of_float y))
        | None -> ()
      in
      match Event.type_ evt with
      | "pointerdown" -> move (); process (E.EMouseButton true)
      | "pointermove" -> move ()
      | "pointerup" ->
          move ();
          process (E.EMouseButton false);
          let now = Date.now () and (t, x, y) = !last_tap in
          if now -. t < 300. && Float.abs (cx -. x) < 30. && Float.abs (cy -. y) < 30. then begin
            process E.EMouseDouble;
            last_tap := (-1000., 0., 0.)
          end
          else last_tap := (now, cx, cy)
      | _ -> ()
    end
  in
  List.iter
    (fun kind ->
      ignore
        (Ojs.call Ojs.global "addEventListener"
           [| Ojs.string_to_js kind; Ojs.fun_to_js 1 (fun e -> on_pointer (Event.t_of_js e)); Ojs.bool_to_js true |]))
    [ "pointerdown"; "pointermove"; "pointerup" ]
