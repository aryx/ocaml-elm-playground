open Playground

let app =
  picture [
    circle lightYellow 200.;
  ]


open Js_browser

let run () = Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child (Document.body document)
let () = Window.set_onload window run

  