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
}

type t = {
  engine : Js_eval.t;
  root : node;
  mutable changed : bool;
  mutable console : string list; (* the newest first *)
  log : string -> unit;
  nodes : (int, node) Hashtbl.t; (* a host object's id to its node *)
}

let text_name = "#text"
let is_text (n : node) : bool = n.name = text_name
let make ?(text = "") ?(attributes = []) (name : string) : node = { name; text; attributes; children = []; parent = None; expando = []; wrapper = None }

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
  | _ -> Option.value (List.assoc_opt k n.expando) ~default:Undefined

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
          | "createTextNode" -> method_ k (fun args -> wrap t (make text_name ~text:(str (arg args 0))))
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

let create ?(seed = 1) ?(log = fun _ -> ()) (tree : Dom.element) : t =
  let lines = ref (fun (_ : string) -> ()) in
  let engine = Js_eval.create ~log:(fun l -> !lines l) ~seed () in
  let t = { engine; root = thaw tree; changed = false; console = []; log; nodes = Hashtbl.create 64 } in
  lines := say t;
  Js_eval.define engine "document" (document t);
  t

let eval (t : t) (text : string) : (value, Js_eval.error) result =
  let r = Js_eval.eval t.engine text in
  (match r with Error e -> report t e | Ok _ -> ());
  r

let run_scripts (t : t) : unit =
  List.iter
    (fun (s : node) ->
      match attribute s "src" with
      | Some src -> say t (Printf.sprintf "<script src=\"%s\"> not loaded: scripts of their own file are an exercise" src)
      | None -> ignore (eval t (text_content s)))
    (List.filter (fun e -> e.name = "script") (elements t.root))

let tree (t : t) : Dom.element =
  t.changed <- false;
  freeze t.root

let changed (t : t) : bool = t.changed
let console (t : t) : string list = List.rev t.console
let engine (t : t) : Js_eval.t = t.engine
