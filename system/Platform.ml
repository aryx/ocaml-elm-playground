
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
