open Vdom

type model = string

let update _ = function
  | `Click -> 
      "Click:"
  | `Keydown s -> 
      "Keydown:" ^ s

let init = "EMPTY"

let view s =
  div
    ~a:[
    onkeydown (fun evt -> `Keydown (Printf.sprintf "%d" evt.which));
    autofocus;
  ]
    [
      div [text (Printf.sprintf "Content: %s" s)];
      div [input [] ~a:[onclick (fun _ -> `Click); type_button; value "Update"]]
    ]

let app = simple_app ~init ~view ~update ()


open Js_browser

let run () = Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child (Document.body document)
let () = Window.set_onload window run
