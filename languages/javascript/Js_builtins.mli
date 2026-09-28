(* Js_builtins: what a script finds already there -- console, Math,
   String, Number, parseInt, JSON, Object.keys -- and the methods of
   strings and arrays.

   (notes_javascript.md section 8.) They are host functions, OCaml
   functions the engine calls like the script's own. A method call on a
   string or an array ("abc".toUpperCase(), xs.map(f)) finds its method
   in its kind's **prototype** object, [protos] (String.prototype,
   Array.prototype), at the end of the chain Js_eval walks: a script can
   read them, call them on other things, and add its own (TinyChrome's
   C8; class, the exercise left).

     console      log (its arguments shown, joined by spaces), error, warn
     Math         floor ceil round trunc abs sign sqrt pow min max random PI
     String, Number, Boolean, parseInt, parseFloat, isNaN
     JSON         stringify
     Object.keys, create, getPrototypeOf, assign; Array.isArray, from
     RegExp, Date (Date.now, its getters), Error and its kinds
     encodeURIComponent, decodeURIComponent, encodeURI, String.fromCharCode
     strings      toUpperCase toLowerCase slice substring charAt indexOf includes
                  startsWith endsWith split trim repeat padStart concat
                  lastIndexOf charCodeAt substr match replace search (with
                  a regular expression or a string), split by one
     arrays       push pop shift unshift join indexOf includes slice concat
                  reverse sort forEach map filter reduce find findIndex some every
                  splice lastIndexOf

   Math.random is **seeded** (Lehmer's generator, the Playground's), so
   that a page of TinyFirefox draws the same numbers each run and a
   golden frame stays golden.

   A function given to map, forEach, sort ... is called back through
   [call], the interpreter's (Js_eval), passed in rather than named, so
   that this module needs nothing of the interpreter. *)

(* the prototypes: the objects a property not an object's own is looked
 * for in next -- a string's, an array's, a plain object's
 * (Object.prototype: hasOwnProperty, toString), a function's
 * (Function.prototype: call, apply, bind), a regular expression's
 * (exec, test), a number's (toFixed) -- each also its constructor's
 * prototype property (String.prototype, Array.prototype...), where a
 * script finds them, calls them on other things (hn.js's
 * Array.prototype.indexOf.call(a, x)), or adds its own *)
type protos = { strings : Js_value.obj; arrays : Js_value.obj; objects : Js_value.obj; functions : Js_value.obj; regexps : Js_value.obj; numbers : Js_value.obj }

(* a RegExp object of [re], its prototype [proto]: a literal's *)
val regexp_value : Js_value.obj -> Js_regexp.t -> Js_value.value

(* [install ~call ~log ~seed ?now define]: every global [define]d,
 * console writing to [log], Math.random from [seed], Date's clock [now]
 * (milliseconds since 1970; 0 unless given); the prototypes *)
val install :
  call:(Js_value.value -> this:Js_value.value -> Js_value.value list -> Js_value.value) ->
  log:(string -> unit) ->
  seed:int ->
  ?now:(unit -> float) ->
  (string -> Js_value.value -> unit) ->
  protos
