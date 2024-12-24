OCaml Elm Playground
=======================

Create pictures, animations, and games with OCaml!

This is a port of the excellent Elm playground package
https://github.com/evancz/elm-playground to OCaml.

> "This is the package I wanted when I was learning programming. Start by
> putting shapes on screen and work up to making games. I hope this
> package will be fun for a broad range of ages and backgrounds!"
> *- Evan Czaplicki*


Documentation
---------------------------------------------------

* [Getting started](https://github.com/aryx/ocaml-elm-playground?tab=readme-ov-file#ocaml-elm-playground)
* [Basic examples](https://aryx.github.io/ocaml-elm-playground/examples/)
* [Basic games](https://aryx.github.io/ocaml-elm-playground/games/)
* [API reference](https://aryx.github.io/ocaml-elm-playground/)

Features
--------------

The OCaml `elm_playground` package allows you to easily create
pictures, animations, and even video games using an API that really
simplifies how to view the computer and its devices (the screen,
keyboard, and mouse).

The main API is defined in a single
[Playground.mli](https://github.com/aryx/ocaml-elm-playground/blob/master/playground/Playground.mli) and is implemented by two backends:
 - a native (SDL-based) backend to run your game in a terminal
 - a web (vdom-based) backend to run your game in a browser

Here is for example a simple [Snake game](https://aryx.github.io/ocaml-elm-playground/games/Snake.html) you can run from your browser.

Install
--------------

To install the playground, run `opam install elm_playground` and then
install one or both backends with `opam install elm_playground_native`
and/or `opam install elm_playground_web`.

Simple native application
--------------------------

Simple web application
--------------------------

