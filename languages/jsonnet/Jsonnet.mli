(* Jsonnet: jsonnet (Google, 2014; https://jsonnet.org), JSON with
   variables, functions, conditionals, comprehensions, imports and
   objects that inherit -- what semgrep writes its rules in, and tinybox's
   code map its .codemapconfig (plan_codemap_v2.md). A program is
   evaluated to a value, manifested as JSON (Json.t).

   The official spec (https://jsonnet.org/ref/spec.html) is the reference,
   not ojsonnet (the author's, in semgrep, which has bugs and departs from
   it). Read by Jsonnet_lexer and Jsonnet_parse, evaluated here, lazily --
   an array's elements, a function's arguments, a local, an object's fields are computed only
   when needed, so a field never asked for may be an error.

   Objects are layers. { a: 1 } is one; o + p puts p's layers over o's
   (e { ... } is e + { ... }). A field is the highest layer's that has
   it, computed with self the whole object -- late binding, so a field
   of o that says self.b sees p's b -- and super the layers under its
   own; f+: v is super.f + v. A field :: is hidden from the output, :::
   shown even if hidden below, : as it was below (visible at first).

     local base = { x: 1, y: self.x + 1, name:: 'base' };
     base + { x: 10, z: super.y }

   is {"x": 10, "y": 11, "z": 11}: y seen from the whole object, where x
   is 10; and name hidden.

   The standard library, std, is the part a configuration needs: length,
   type, toString, join, map, filter, foldl, foldr, range, makeArray,
   objectFields(All), objectHas(All), objectValues, mapWithKey, get,
   format (and %), startsWith, endsWith, split, strReplace, substr,
   stringChars, asciiUpper, asciiLower, member, count, flattenArrays,
   sort, uniq, reverse, find, abs, min, max, floor, ceil, pow, sqrt,
   isString, isNumber, isBoolean, isArray, isObject, isFunction,
   parseInt, mergePatch, assertEqual, trace; grown as configurations
   need more.

   Where it departs from the spec, knowingly (enough for configurations,
   2026-09-29): importbin, std.extVar and the rest of std are missing; an
   object's asserts are checked when it is manifested, not at every
   field; a string's length and index count bytes, not code points;
   numbers are printed as OCaml's %.17g, not always as jsonnet does.
   The spec's own test suite (the google/jsonnet repository's test_suite/)
   is how to find the rest, when it matters.

   An import's file is read by [read] (its path, relative to the root
   the host chose; resolved from the importing file's by Jsonnet_parse),
   so the evaluator stays pure: tinybox reads from its embedded sources,
   the tests from a list. *)

(* [eval ?read ~path text]: the JSON [text] (the file [path]'s) stands
   for, or the error ("path:line: what"); [read] gives an imported
   file's text (none by default) *)
val eval : ?read:(string -> string option) -> path:string -> string -> (Json.t, string) result
