(* was called Browser in Elm, not UI, but we try to be more general *)

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
