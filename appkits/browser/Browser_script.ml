(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_script.mli *)
open Js_value

(*****************************************************************************)
(* The copy: nodes with a parent *)
(*****************************************************************************)

(* an element, or a text ([name] "#text", its [text]) *)
type node = {
  name : string;
  mutable text : string;
  mutable attributes : (string * string) list; (* all of them, core and Netscape's *)
  mutable children : node list;
  mutable parent : node option;
  mutable expando : (string * value) list; (* what a script set on it: el.done = true *)
  mutable wrapper : value option; (* its host object, made once *)
  mutable listeners : (string * value) list; (* addEventListener's, in order *)
  mutable compiled : (string * value) list; (* its onclick="..." attributes, compiled once *)
}

(* a setTimeout's or a setInterval's *)
type timer = { tid : int; mutable due : float; every : float option; fn : value }

type t = {
  engine : Js_eval.t;
  root : node;
  mutable changed : bool;
  mutable console : string list; (* the newest first *)
  log : string -> unit;
  nodes : (int, node) Hashtbl.t; (* a host object's id to its node *)
  mutable document_listeners : (string * value) list;
  mutable frozen : (Dom.element * node) list; (* the last frozen tree's elements, and their nodes *)
  mutable now : float; (* the page's clock, in ms *)
  mutable timers : timer list;
  mutable next_timer : int;
  mutable alerts : string list; (* the newest first *)
  base : string; (* the page's address: an a's href resolved, location, new URL *)
  mutable requests : string list; (* XMLHttpRequest's and fetch's GETs, for the browser to send; the newest first *)
}

let text_name = "#text"
let is_text (n : node) : bool = n.name = text_name
let make ?(text = "") ?(attributes = []) (name : string) : node =
  { name; text; attributes; children = []; parent = None; expando = []; wrapper = None; listeners = []; compiled = [] }

let rec thaw (e : Dom.element) : node =
  let n = make e.name ~attributes:(e.attributes @ e.extensions) in
  n.children <-
    List.map
      (fun (c : Dom.node) ->
        let child = match c with Element e -> thaw e | Text s -> make text_name ~text:s in
        child.parent <- Some n;
        child)
      e.children;
  n

(* back into a Dom value: Netscape's attributes apart again (Dtd.origin),
 * as the lexer puts them *)
let rec freeze (n : node) : Dom.element =
  let origin = Dtd.element_origin n.name in
  let attributes, extensions =
    match origin with
    | Netscape -> (n.attributes, [])
    | Core -> List.partition (fun a -> Dtd.attribute_origin n.name a = Dtd.Core) n.attributes
  in
  let children = List.map (fun c -> if is_text c then Dom.Text c.text else Dom.Element (freeze c)) n.children in
  { name = n.name; attributes; extensions; origin; children }

(* every element under [n] ([n] too), in document order *)
let rec elements (n : node) : node list = if is_text n then [] else n :: List.concat_map elements n.children

let rec text_content (n : node) : string = if is_text n then n.text else String.concat "" (List.map text_content n.children)
let attribute (n : node) (k : string) : string option = List.assoc_opt k n.attributes

let set_attribute (n : node) (k : string) (v : string) : unit =
  n.attributes <- (if List.mem_assoc k n.attributes then List.map (fun (a, x) -> if a = k then (a, v) else (a, x)) n.attributes else n.attributes @ [ (k, v) ])

let detach (n : node) : unit =
  Option.iter (fun p -> p.children <- List.filter (fun c -> c != n) p.children) n.parent;
  n.parent <- None

let adopt (parent : node) (children : node list) : unit = List.iter (fun c -> c.parent <- Some parent) children

(*****************************************************************************)
(* HTML: innerHTML read and written *)
(*****************************************************************************)

let escape ?(quote = false) (s : string) : string =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' when quote -> Buffer.add_string b "&quot;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let rec html_of (n : node) : string =
  if is_text n then escape n.text
  else
    let attrs = String.concat "" (List.map (fun (k, v) -> Printf.sprintf " %s=\"%s\"" k (escape ~quote:true v)) n.attributes) in
    Printf.sprintf "<%s%s>%s%s" n.name attrs (inner_html n) (if Dtd.is_void n.name then "" else "</" ^ n.name ^ ">")

and inner_html (n : node) : string = String.concat "" (List.map html_of n.children)

(* a fragment's nodes: the parser makes a whole page of it, whose head
 * holds what belongs there (a <style>) and whose body the rest *)
let parse_fragment (s : string) : node list =
  let page = thaw (Html_tree.of_string s) in
  List.concat_map (fun (part : node) -> part.children) page.children

(*****************************************************************************)
(* Selectors *)
(*****************************************************************************)

(* the elements matching a selector, in document order, among those
 * under [within]: matched on the frozen tree, with its ancestors, as
 * the page's style sheets are (Css.matches) *)
let select (t : t) (selector : string) ~(within : node) : node list =
  match Css.parse (selector ^ " {}") with
  | [] -> throw "SyntaxError" (Printf.sprintf "'%s' is not a valid selector" selector)
  | rules ->
      let inside = elements within in
      let found = ref [] in
      let rec go (ancestors : Dom.element list) (n : node) =
        if not (is_text n) then (
          let e = freeze n in
          if List.memq n inside && n != within && List.exists (fun (r : Css.rule) -> Css.matches r.selector ancestors e) rules then
            found := n :: !found;
          List.iter (go (e :: ancestors)) n.children)
      in
      go [] t.root;
      List.rev !found

(*****************************************************************************)
(* The host objects *)
(*****************************************************************************)

(* a script has changed the tree: the page to be laid out again *)
let touch (t : t) : unit = t.changed <- true
let str (v : value) : string = to_string v
let arg (args : value list) (i : int) : value = Option.value (List.nth_opt args i) ~default:Undefined

(* the node a host object stands for *)
let node_of (t : t) (v : value) : node =
  match v with
  | Object o -> ( match Hashtbl.find_opt t.nodes o.id with Some n -> n | None -> throw "TypeError" "parameter 1 is not of type 'Node'")
  | _ -> throw "TypeError" "parameter 1 is not of type 'Node'"

(* backgroundColor, the property; background-color, the CSS *)
let kebab (s : string) : string =
  String.concat "" (List.map (fun c -> if c >= 'A' && c <= 'Z' then "-" ^ String.make 1 (Char.lowercase_ascii c) else String.make 1 c) (List.init (String.length s) (String.get s)))

(* el.style: its style= attribute's declarations, read and written one
 * by one *)
let style_object (t : t) (n : node) : value =
  let decls () = match attribute n "style" with Some s -> Css.declarations s | None -> [] in
  host_object
    {
      class_name = "CSSStyleDeclaration";
      get = (fun k -> match List.assoc_opt (kebab k) (decls ()) with Some v -> String v | None -> String "");
      set =
        (fun k v ->
          let k = kebab k and v = str v in
          let others = List.remove_assoc k (decls ()) in
          let all = if v = "" then others else others @ [ (k, v) ] in
          set_attribute n "style" (String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) all));
          touch t);
      show = (fun () -> "CSSStyleDeclaration");
    }

let rec wrap (t : t) (n : node) : value =
  match n.wrapper with
  | Some v -> v
  | None ->
      let v = host_object { class_name = (if is_text n then "Text" else "HTMLElement"); get = get t n; set = set t n; show = (fun () -> show n) } in
      (match v with Object o -> Hashtbl.replace t.nodes o.id n | _ -> ());
      n.wrapper <- Some v;
      v

(* how the console shows an element: its start tag *)
and show (n : node) : string =
  if is_text n then Printf.sprintf "%S" n.text
  else "<" ^ n.name ^ String.concat "" (List.map (fun (k, v) -> Printf.sprintf " %s=\"%s\"" k v) n.attributes) ^ ">"

and nodes_array (t : t) (ns : node list) : value = Object (new_array (List.map (wrap t) ns))

and method_ (name : string) (f : value list -> value) : value = host_function name (fun ~this:_ args -> f args)

and get (t : t) (n : node) (k : string) : value =
  let elements_of ns = List.filter (fun c -> not (is_text c)) ns in
  let opt = function Some c -> wrap t c | None -> Null in
  match k with
  | "tagName" | "nodeName" -> String (if is_text n then "#text" else String.uppercase_ascii n.name)
  | "nodeType" -> Number (if is_text n then 3. else 1.)
  | "id" -> String (Option.value (attribute n "id") ~default:"")
  | "className" -> String (Option.value (attribute n "class") ~default:"")
  | "textContent" | "innerText" | "data" | "nodeValue" -> String (text_content n)
  | "innerHTML" -> String (inner_html n)
  | "outerHTML" -> String (html_of n)
  | "value" -> String (if n.name = "textarea" then text_content n else Option.value (attribute n "value") ~default:"")
  | "checked" -> Bool (attribute n "checked" <> None)
  | "style" -> style_object t n
  | "children" -> nodes_array t (elements_of n.children)
  | "childNodes" -> nodes_array t n.children
  | "firstChild" -> opt (List.nth_opt n.children 0)
  | "lastChild" -> opt (List.nth_opt (List.rev n.children) 0)
  | "firstElementChild" -> opt (List.nth_opt (elements_of n.children) 0)
  | "parentNode" | "parentElement" -> opt n.parent
  (* its siblings: the next or previous node, or element *)
  | "nextSibling" | "previousSibling" | "nextElementSibling" | "previousElementSibling" -> (
      match n.parent with
      | None -> Null
      | Some p ->
          let sibs = if String.ends_with ~suffix:"ElementSibling" k then elements_of p.children else p.children in
          let sibs = if String.starts_with ~prefix:"previous" k then List.rev sibs else sibs in
          let rec after l = match l with x :: y :: _ when x == n -> Some y | _ :: r -> after r | [] -> None in
          opt (after sibs))
  | "getElementsByClassName" ->
      method_ k (fun args ->
          let wanted = List.filter (( <> ) "") (String.split_on_char ' ' (str (arg args 0))) in
          nodes_array t (List.filter (fun e -> e != n && has_classes e wanted) (elements n)))
  | "getElementsByTagName" ->
      method_ k (fun args ->
          let name = String.lowercase_ascii (str (arg args 0)) in
          nodes_array t (List.filter (fun e -> e != n && (name = "*" || e.name = name)) (elements n)))
  | "classList" -> class_list t n
  | "href" when n.name = "a" || n.name = "link" || n.name = "area" -> (
      match attribute n "href" with Some h -> String (Browser_url.resolve t.base h) | None -> String "")
  | "src" when n.name = "img" || n.name = "script" -> ( match attribute n "src" with Some h -> String (Browser_url.resolve t.base h) | None -> String "")
  (* what a page does that has no effect here: nothing to scroll to, no
   * focus to move *)
  | "scrollIntoView" | "focus" | "blur" -> method_ k (fun _ -> Undefined)
  | "insertAdjacentHTML" ->
      method_ k (fun args ->
          let nodes = parse_fragment (str (arg args 1)) in
          (match String.lowercase_ascii (str (arg args 0)) with
          | "beforeend" -> n.children <- n.children @ nodes; adopt n nodes
          | "afterbegin" -> n.children <- nodes @ n.children; adopt n nodes
          | ("beforebegin" | "afterend") as where -> (
              match n.parent with
              | Some p ->
                  p.children <- List.concat_map (fun c -> if c == n then (if where = "beforebegin" then nodes @ [ c ] else c :: nodes) else [ c ]) p.children;
                  adopt p nodes
              | None -> ())
          | _ -> ());
          touch t;
          Undefined)
  | "getAttribute" -> method_ k (fun args -> match attribute n (str (arg args 0)) with Some v -> String v | None -> Null)
  | "setAttribute" ->
      method_ k (fun args ->
          set_attribute n (String.lowercase_ascii (str (arg args 0))) (str (arg args 1));
          touch t;
          Undefined)
  | "removeAttribute" ->
      method_ k (fun args ->
          n.attributes <- List.remove_assoc (str (arg args 0)) n.attributes;
          touch t;
          Undefined)
  | "appendChild" -> method_ k (fun args -> insert t n (arg args 0) ~before:None)
  | "insertBefore" -> method_ k (fun args -> insert t n (arg args 0) ~before:(match arg args 1 with Null | Undefined -> None | v -> Some (node_of t v)))
  | "removeChild" ->
      method_ k (fun args ->
          let c = node_of t (arg args 0) in
          (match c.parent with
          | Some p when p == n -> ()
          | _ -> throw "NotFoundError" "The node to be removed is not a child of this node.");
          detach c;
          touch t;
          arg args 0)
  | "remove" -> method_ k (fun _ -> detach n; touch t; Undefined)
  | "querySelector" -> method_ k (fun args -> opt (List.nth_opt (select t (str (arg args 0)) ~within:n) 0))
  | "querySelectorAll" -> method_ k (fun args -> nodes_array t (select t (str (arg args 0)) ~within:n))
  | "addEventListener" ->
      method_ k (fun args ->
          n.listeners <- n.listeners @ [ (str (arg args 0), arg args 1) ];
          Undefined)
  | "removeEventListener" ->
      method_ k (fun args ->
          let typ = str (arg args 0) and f = arg args 1 in
          n.listeners <- List.filter (fun (ty, g) -> not (ty = typ && strict_equal g f)) n.listeners;
          Undefined)
  | _ -> Option.value (List.assoc_opt k n.expando) ~default:Undefined

(* whether an element has every class of [wanted] *)
and has_classes (e : node) (wanted : string list) : bool =
  let have = match attribute e "class" with Some c -> String.split_on_char ' ' c | None -> [] in
  wanted <> [] && List.for_all (fun w -> List.mem w have) wanted

(* el.classList: its class= as a set of words *)
and class_list (t : t) (n : node) : value =
  let words () = List.filter (( <> ) "") (String.split_on_char ' ' (Option.value (attribute n "class") ~default:"")) in
  let write ws = set_attribute n "class" (String.concat " " ws); touch t in
  host_object
    {
      class_name = "DOMTokenList";
      get =
        (fun k ->
          match k with
          | "length" -> Number (float_of_int (List.length (words ())))
          | "contains" -> method_ k (fun args -> Bool (List.mem (str (arg args 0)) (words ())))
          | "add" -> method_ k (fun args -> write (words () @ List.filter (fun c -> not (List.mem c (words ()))) (List.map str args)); Undefined)
          | "remove" -> method_ k (fun args -> write (List.filter (fun c -> not (List.mem (String c) args || List.exists (fun a -> str a = c) args)) (words ())); Undefined)
          | "toggle" ->
              method_ k (fun args ->
                  let c = str (arg args 0) in
                  if List.mem c (words ()) then (write (List.filter (( <> ) c) (words ())); Bool false) else (write (words () @ [ c ]); Bool true))
          | _ -> Undefined);
      set = (fun _ _ -> ());
      show = (fun () -> String.concat " " (words ()));
    }

and set (t : t) (n : node) (k : string) (v : value) : unit =
  let replace_children (cs : node list) =
    List.iter (fun c -> c.parent <- None) n.children;
    n.children <- cs;
    adopt n cs;
    touch t
  in
  match k with
  | "id" -> set_attribute n "id" (str v); touch t
  | "className" -> set_attribute n "class" (str v); touch t
  | "textContent" | "innerText" | "data" | "nodeValue" ->
      if is_text n then (n.text <- str v; touch t) else replace_children [ make text_name ~text:(str v) ]
  | "innerHTML" -> replace_children (parse_fragment (str v))
  | "value" -> if n.name = "textarea" then replace_children [ make text_name ~text:(str v) ] else (set_attribute n "value" (str v); touch t)
  | "checked" ->
      (if truthy v then set_attribute n "checked" "" else n.attributes <- List.remove_assoc "checked" n.attributes);
      touch t
  | _ -> n.expando <- (k, v) :: List.remove_assoc k n.expando

(* child put in [parent], before [before] or at the end: moved if it
 * was elsewhere; never inside itself *)
and insert (t : t) (parent : node) (child_v : value) ~(before : node option) : value =
  let child = node_of t child_v in
  let rec inside (p : node option) = match p with Some p -> p == child || inside p.parent | None -> false in
  if inside (Some parent) then throw "HierarchyRequestError" "The new child element contains the parent.";
  detach child;
  (parent.children <-
     match before with
     | None -> parent.children @ [ child ]
     | Some b -> List.concat_map (fun c -> if c == b then [ child; c ] else [ c ]) parent.children);
  child.parent <- Some parent;
  touch t;
  child_v

(* the first element named so, at any depth *)
let find (n : node) (name : string) : node option = List.find_opt (fun e -> e.name = name) (elements n)

(* a URL's parts, as location and URL give them: href, protocol, host,
 * pathname, search, hash... *)
let url_parts (href : string) : (string * string) list =
  let without_hash, hash = match String.index_opt href '#' with Some i -> (String.sub href 0 i, String.sub href i (String.length href - i)) | None -> (href, "") in
  let without_query, search =
    match String.index_opt without_hash '?' with Some i -> (String.sub without_hash 0 i, String.sub without_hash i (String.length without_hash - i)) | None -> (without_hash, "")
  in
  let protocol, rest = match String.index_opt without_query ':' with Some i -> (String.sub without_query 0 (i + 1), String.sub without_query (i + 1) (String.length without_query - i - 1)) | None -> ("", without_query) in
  let host, pathname =
    if String.starts_with ~prefix:"//" rest then
      let r = String.sub rest 2 (String.length rest - 2) in
      match String.index_opt r '/' with Some i -> (String.sub r 0 i, String.sub r i (String.length r - i)) | None -> (r, "/")
    else ("", rest)
  in
  let hostname = match String.index_opt host ':' with Some i -> String.sub host 0 i | None -> host in
  [ ("href", href); ("protocol", protocol); ("host", host); ("hostname", hostname); ("pathname", pathname); ("search", search); ("hash", hash);
    ("origin", if host = "" then "null" else protocol ^ "//" ^ host) ]

(* an object of a URL's parts; URL's searchParams, and toString *)
let url_object (href : string) : value =
  let parts = url_parts href in
  let o = new_object () in
  List.iter (fun (k, v) -> set_own o k (String v)) parts;
  let query = let s = List.assoc "search" parts in if s = "" then "" else String.sub s 1 (String.length s - 1) in
  let params = Urlencoded.decode query in
  let sp = new_object () in
  set_own sp "get" (host_function "get" (fun ~this:_ args -> match List.assoc_opt (str (arg args 0)) params with Some v -> String v | None -> Null));
  set_own sp "has" (host_function "has" (fun ~this:_ args -> Bool (List.mem_assoc (str (arg args 0)) params)));
  set_own o "searchParams" (Object sp);
  set_own o "toString" (host_function "toString" (fun ~this:_ _ -> String href));
  Object o

let location (t : t) : value = url_object t.base

let document (t : t) : value =
  let root = t.root in
  let title () = find root "title" in
  host_object
    {
      class_name = "HTMLDocument";
      get =
        (fun k ->
          match k with
          | "body" -> ( match find root "body" with Some b -> wrap t b | None -> Null)
          | "head" -> ( match find root "head" with Some h -> wrap t h | None -> Null)
          | "documentElement" -> wrap t root
          | "title" -> String (match title () with Some n -> String.trim (text_content n) | None -> "")
          | "getElementById" ->
              method_ k (fun args ->
                  let id = str (arg args 0) in
                  match List.find_opt (fun e -> attribute e "id" = Some id) (elements root) with Some e -> wrap t e | None -> Null)
          | "querySelector" -> method_ k (fun args -> match select t (str (arg args 0)) ~within:root with e :: _ -> wrap t e | [] -> Null)
          | "querySelectorAll" -> method_ k (fun args -> nodes_array t (select t (str (arg args 0)) ~within:root))
          | "createElement" -> method_ k (fun args -> wrap t (make (String.lowercase_ascii (str (arg args 0)))))
          | "getElementsByClassName" | "getElementsByTagName" -> get t root k
          | "location" -> location t
          | "URL" -> String t.base
          | "cookie" | "referrer" -> String ""
          | "readyState" -> String "complete"
          | "defaultView" -> Option.value (Js_eval.global t.engine "window") ~default:Undefined
          | "createTextNode" -> method_ k (fun args -> wrap t (make text_name ~text:(str (arg args 0))))
          | "addEventListener" ->
              method_ k (fun args ->
                  t.document_listeners <- t.document_listeners @ [ (str (arg args 0), arg args 1) ];
                  Undefined)
          | "removeEventListener" ->
              method_ k (fun args ->
                  let typ = str (arg args 0) and f = arg args 1 in
                  t.document_listeners <- List.filter (fun (ty, g) -> not (ty = typ && strict_equal g f)) t.document_listeners;
                  Undefined)
          | _ -> Undefined);
      set =
        (fun k v ->
          match (k, title ()) with
          | "title", Some n ->
              n.children <- [ make text_name ~text:(str v) ];
              adopt n n.children;
              touch t
          | "title", None -> (
              match find root "head" with
              | Some h ->
                  let n = make "title" in
                  n.children <- [ make text_name ~text:(str v) ];
                  adopt n n.children;
                  h.children <- h.children @ [ n ];
                  adopt h [ n ];
                  touch t
              | None -> ())
          | _ -> ());
      show = (fun () -> "#document");
    }

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let say (t : t) (line : string) : unit =
  t.console <- line :: t.console;
  t.log line

let report (t : t) (e : Js_eval.error) : unit =
  let m = e.message in
  say t (Printf.sprintf "%s (line %d)" (if String.starts_with ~prefix:"Uncaught" m then m else "Uncaught " ^ m) e.line)

(*****************************************************************************)
(* Events *)
(*****************************************************************************)

(* a handler run, a task of its own: its error in the console, not the
 * next handler's business; whether it returned false (DOM level 0's way
 * to cancel, onclick="...; return false") *)
let run_handler (t : t) (f : value) ~(this : value) (event : value) : bool =
  match Js_eval.call t.engine f ~this [ event ] with
  | Ok (Bool false) -> true
  | Ok _ -> false
  | Error e -> report t e; false

(* onclick="..." compiled once into a function of event, as browsers
 * do: the attribute's text is the function's body *)
let attribute_handler (t : t) (n : node) (typ : string) : value option =
  match attribute n ("on" ^ typ) with
  | None -> None
  | Some src -> (
      match List.assoc_opt src n.compiled with
      | Some f -> Some f
      | None -> (
          match Js_eval.eval t.engine ("(function (event) {\n" ^ src ^ "\n})") with
          | Ok f ->
              n.compiled <- (src, f) :: n.compiled;
              Some f
          | Error e -> report t e; None))

(* an event dispatched at [target]: its handlers, then its parent's, up
 * to the document (bubbling), unless one stops it; whether one
 * prevented the default *)
let dispatch (t : t) (target : node) (typ : string) (fields : (string * value) list) : bool =
  let prevented = ref false and stopped = ref false and stopped_now = ref false in
  let ev = new_object () in
  set_own ev "type" (String typ);
  set_own ev "target" (wrap t target);
  List.iter (fun (k, v) -> set_own ev k v) fields;
  set_own ev "defaultPrevented" (Bool false);
  set_own ev "preventDefault" (host_function "preventDefault" (fun ~this:_ _ -> prevented := true; set_own ev "defaultPrevented" (Bool true); Undefined));
  set_own ev "stopPropagation" (host_function "stopPropagation" (fun ~this:_ _ -> stopped := true; Undefined));
  (* and the other handlers of the same element not run either *)
  set_own ev "stopImmediatePropagation" (host_function "stopImmediatePropagation" (fun ~this:_ _ -> stopped := true; stopped_now := true; Undefined));
  let event = Object ev in
  let handle this (listeners : (string * value) list) (extra : value option) =
    set_own ev "currentTarget" this;
    List.iter (fun (ty, f) -> if ty = typ && (not !stopped_now) && run_handler t f ~this event then prevented := true) listeners;
    Option.iter (fun f -> if (not !stopped_now) && run_handler t f ~this event then prevented := true) extra
  in
  let rec up (n : node option) =
    match n with
    | Some n when not !stopped ->
        (* el.onclick = f, else onclick="..." *)
        let on = match List.assoc_opt ("on" ^ typ) n.expando with Some (Object _ as f) -> Some f | _ -> attribute_handler t n typ in
        handle (wrap t n) n.listeners on;
        up n.parent
    | _ -> ()
  in
  up (Some target);
  if not !stopped then handle (Js_eval.global t.engine "document" |> Option.value ~default:Undefined) t.document_listeners None;
  !prevented

(* the node a frozen element came from *)
let node_of_element (t : t) (e : Dom.element) : node option = List.find_map (fun (e', n) -> if e' == e then Some n else None) t.frozen

(*****************************************************************************)
(* Timers *)
(*****************************************************************************)

let add_timer (t : t) (args : value list) ~(repeat : bool) : value =
  let f = arg args 0 in
  (* 1 ms at least: a setInterval(f, 0) must let the clock move *)
  let ms = Float.max 1. (match arg args 1 with Undefined -> 0. | v -> to_number v) in
  t.next_timer <- t.next_timer + 1;
  t.timers <- t.timers @ [ { tid = t.next_timer; due = t.now +. ms; every = (if repeat then Some ms else None); fn = f } ];
  Number (float_of_int t.next_timer)

let clear_timer (t : t) (args : value list) : value =
  let id = int_of_float (to_number (arg args 0)) in
  t.timers <- List.filter (fun tm -> tm.tid <> id) t.timers;
  Undefined

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let create ?(seed = 1) ?(log = fun _ -> ()) ?(base = "about:blank") ?(epoch = 0.) ?(viewport = (1000., 768.)) (tree : Dom.element) : t =
  let lines = ref (fun (_ : string) -> ()) in
  let clock = ref (fun () -> epoch) in
  let engine = Js_eval.create ~log:(fun l -> !lines l) ~seed ~now:(fun () -> !clock ()) () in
  let t =
    { engine; root = thaw tree; changed = false; console = []; log; nodes = Hashtbl.create 64; document_listeners = []; frozen = [];
      now = 0.; timers = []; next_timer = 0; alerts = []; base; requests = [] }
  in
  (* Date's clock: the page's, from [epoch] *)
  clock := (fun () -> epoch +. t.now);
  lines := say t;
  let define name f = Js_eval.define engine name (host_function name (fun ~this:_ args -> f args)) in
  Js_eval.define engine "document" (document t);
  define "setTimeout" (fun args -> add_timer t args ~repeat:false);
  define "setInterval" (fun args -> add_timer t args ~repeat:true);
  define "clearTimeout" (clear_timer t);
  define "clearInterval" (clear_timer t);
  define "alert" (fun args -> t.alerts <- str (arg args 0) :: t.alerts; Undefined);
  (* window: the global object -- a global read or set through it; its
   * listeners the document's, its size the window's *)
  let window =
    host_object
      {
        class_name = "Window";
        get =
          (fun k ->
            match k with
            | "innerWidth" -> Number (fst viewport)
            | "innerHeight" -> Number (snd viewport)
            | "location" -> location t
            | "document" -> Option.value (Js_eval.global engine "document") ~default:Undefined
            | "addEventListener" | "removeEventListener" -> (
                match Js_eval.global engine "document" with Some (Object { kind = Host_object h; _ }) -> h.get k | _ -> Undefined)
            | "scrollTo" | "scrollBy" -> method_ k (fun _ -> Undefined)
            | k -> Option.value (Js_eval.global engine k) ~default:Undefined);
        set = (fun k v -> Js_eval.define engine k v);
        show = (fun () -> "Window");
      }
  in
  Js_eval.define engine "window" window;
  Js_eval.define engine "self" window;
  Js_eval.define engine "location" (location t);
  Js_eval.define engine "navigator"
    (let o = new_object () in
     set_own o "userAgent" (String "Mozilla/5.0 (TinyChrome; elm_playground)");
     set_own o "language" (String "en-US");
     Object o);
  (* new URL(href, base) *)
  define "URL" (fun args ->
      let base = match arg args 1 with Undefined -> t.base | v -> str v in
      url_object (Browser_url.resolve base (str (arg args 0))));
  (* a GET queued for the browser to send ([take_requests]); its answer
   * not given back (no onload): enough for a vote, not for a page that
   * reads what it asked *)
  let queue url = t.requests <- Browser_url.resolve t.base url :: t.requests in
  define "XMLHttpRequest" (fun _ ->
      let url = ref None in
      let o = new_object () in
      set_own o "readyState" (Number 0.);
      set_own o "open" (host_function "open" (fun ~this:_ args -> url := Some (str (arg args 1)); Undefined));
      set_own o "setRequestHeader" (host_function "setRequestHeader" (fun ~this:_ _ -> Undefined));
      set_own o "send" (host_function "send" (fun ~this:_ _ -> Option.iter queue !url; Undefined));
      Object o);
  (* fetch: the GET queued, a promise that never settles (no promises
   * here: its then's are kept, never called) *)
  define "fetch" (fun args ->
      queue (str (arg args 0));
      let p = new_object () in
      let self = Object p in
      set_own p "then" (host_function "then" (fun ~this:_ _ -> self));
      set_own p "catch" (host_function "catch" (fun ~this:_ _ -> self));
      self);
  t

let eval (t : t) (text : string) : (value, Js_eval.error) result =
  let r = Js_eval.eval t.engine text in
  (match r with Error e -> report t e | Ok _ -> ());
  r

(* a script the page's: JavaScript by its type= (not JSON-LD, not a
 * module, not a template) *)
let runnable (s : node) : bool =
  match Option.map String.lowercase_ascii (attribute s "type") with
  | None | Some "" | Some "text/javascript" | Some "application/javascript" -> true
  | Some _ -> false

let script_sources (t : t) : string list =
  List.filter_map
    (fun (s : node) -> if runnable s then Option.map (Browser_url.resolve t.base) (attribute s "src") else None)
    (List.filter (fun e -> e.name = "script") (elements t.root))

let run_scripts ?(source = fun (_ : string) -> None) (t : t) : unit =
  List.iter
    (fun (s : node) ->
      match attribute s "src" with
      | Some src -> (
          match source (Browser_url.resolve t.base src) with
          | Some text -> ignore (eval t text)
          | None -> say t (Printf.sprintf "<script src=\"%s\"> could not be had" src))
      | None -> ignore (eval t (text_content s)))
    (List.filter (fun e -> e.name = "script" && runnable e) (elements t.root));
  (* then the document is loaded: its listeners told *)
  List.iter
    (fun typ ->
      let listeners = List.filter (fun (ty, _) -> ty = typ) t.document_listeners in
      List.iter (fun (_, f) -> ignore (run_handler t f ~this:Undefined Undefined)) listeners)
    [ "DOMContentLoaded"; "load" ]

let tree (t : t) : Dom.element =
  t.changed <- false;
  let pairs = ref [] in
  let rec go (n : node) : Dom.element =
    let origin = Dtd.element_origin n.name in
    let attributes, extensions =
      match origin with
      | Netscape -> (n.attributes, [])
      | Core -> List.partition (fun a -> Dtd.attribute_origin n.name a = Dtd.Core) n.attributes
    in
    let children = List.map (fun c -> if is_text c then Dom.Text c.text else Dom.Element (go c)) n.children in
    let e : Dom.element = { name = n.name; attributes; extensions; origin; children } in
    pairs := (e, n) :: !pairs;
    e
  in
  let root = go t.root in
  t.frozen <- !pairs;
  root

let click (t : t) (e : Dom.element) : bool =
  match node_of_element t e with Some n -> dispatch t n "click" [] | None -> false

let key (t : t) (k : string) : bool =
  let body = match List.find_opt (fun n -> n.name = "body") (elements t.root) with Some b -> b | None -> t.root in
  dispatch t body "keydown" [ ("key", String k) ]

let input (t : t) (e : Dom.element) (text : string) : unit =
  match node_of_element t e with
  | Some n ->
      set_attribute n "value" text;
      touch t;
      ignore (dispatch t n "input" [])
  | None -> ()

let advance (t : t) (ms : float) : unit =
  t.now <- t.now +. ms;
  (* the timers due, the earliest first, each a task; an interval put
   * back at its next time; a thousand at most, so that a page cannot
   * keep the browser here *)
  let rec go (runs : int) =
    match List.sort (fun a b -> compare (a.due, a.tid) (b.due, b.tid)) (List.filter (fun tm -> tm.due <= t.now) t.timers) with
    | tm :: _ when runs < 1000 ->
        (match tm.every with
        | Some every -> tm.due <- tm.due +. every
        | None -> t.timers <- List.filter (fun x -> x.tid <> tm.tid) t.timers);
        ignore (run_handler t tm.fn ~this:Undefined Undefined);
        go (runs + 1)
    | _ -> ()
  in
  go 0

let take_requests (t : t) : string list =
  let r = List.rev t.requests in
  t.requests <- [];
  r

let take_alerts (t : t) : string list =
  let a = List.rev t.alerts in
  t.alerts <- [];
  a

let changed (t : t) : bool = t.changed
let console (t : t) : string list = List.rev t.console
let print (t : t) (line : string) : unit = say t line
let engine (t : t) : Js_eval.t = t.engine
