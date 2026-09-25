(* Js_builtins: what a script finds already there -- console, Math,
   String, Number, parseInt, JSON, Object.keys -- and the methods of
   strings and arrays.

   (notes_javascript.md section 8.) They are host functions, OCaml
   functions the engine calls like the script's own. A method call on a
   string or an array ("abc".toUpperCase(), xs.map(f)) finds its method
   in a table per kind, [protos]: where a real engine looks in a
   **prototype** object (String.prototype, Array.prototype), which the
   script could change too -- the exercise that brings prototypes, new
   and class with it.

     console      log (its arguments shown, joined by spaces), error, warn
     Math         floor ceil round trunc abs sign sqrt pow min max random PI
     String, Number, Boolean, parseInt, parseFloat, isNaN
     JSON         stringify
     Object.keys, Array.isArray
     strings      toUpperCase toLowerCase slice substring charAt indexOf includes
                  startsWith endsWith split trim repeat padStart concat
     arrays       push pop shift unshift join indexOf includes slice concat
                  reverse sort forEach map filter reduce find findIndex some every

   Math.random is **seeded** (Lehmer's generator, the Playground's), so
   that a page of TinyFirefox draws the same numbers each run and a
   golden frame stays golden.

   A function given to map, forEach, sort ... is called back through
   [call], the interpreter's (Js_eval), passed in rather than named, so
   that this module needs nothing of the interpreter. *)

(* the methods of strings and of arrays, looked up by name *)
type protos = { strings : Js_value.obj; arrays : Js_value.obj }

(* [install ~call ~log ~seed define]: every global [define]d, console
 * writing to [log], Math.random from [seed]; the methods' tables *)
val install :
  call:(Js_value.value -> this:Js_value.value -> Js_value.value list -> Js_value.value) ->
  log:(string -> unit) ->
  seed:int ->
  (string -> Js_value.value -> unit) ->
  protos
