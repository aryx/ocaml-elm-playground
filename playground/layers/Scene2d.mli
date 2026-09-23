(* Scenes: the title screen, the game, the game over screen.

   A game is more than its play: an arcade game shows a title (and waits
   for a coin), then plays, then says GAME OVER and shows the high
   scores, and goes back to the title. Each of these screens is a scene,
   with its own view and its own update; the game's model says which
   scene it is in:

     type scene = Title | Playing of game | Game_over of int (* score *)

         +-------+  space   +---------+  no lives  +-----------+
     --> | Title | -------> | Playing | ---------> | Game_over |
         +-------+          +---------+            +-----------+
             ^                                           |
             +-------------------------------------------+
                           space, or after 10 seconds

   That's all a scene is, in Elm: a variant in the model, a [match] in
   [view] and [update]. This module adds the two things every such game
   needs and the playground doesn't give:
     - time spent in the current scene ([elapsed], [frames]): to show
       "PRESS SPACE" blinking, to go back to the title after 10 seconds;
     - keys *pressed*, not held ([pressed]): computer.keyboard says which
       keys are down, so a player still holding space from the title
       would skip the game over screen in one frame; what a scene
       change wants is the moment a key goes down, its "rising edge"
       (the word of electronics, where a signal goes from 0 to 1), which
       takes remembering the previous frame's keyboard.
   Both live in a ['scene t] wrapped around the game's own scene type:

     type model = scene Scene2d.t
     let update computer model =
       let model = Scene2d.update computer model in
       match model.scene with
       | Title when Scene2d.pressed (fun k -> k.kspace) model ->
           Scene2d.go (Playing new_game) model
       | ...

   This module is a layer on top of Playground (it only reads the
   computer), like Camera2d.

   A bit of history. Arcade games invented the scenes to earn coins: the
   "attract mode", a title and a demo playing by itself while nobody
   plays, to lure passers-by; the high score table, which Space
   Invaders (1978) kept between players, and to which Star Fire and
   Asteroids (1979) added the players' initials: three letters for all
   the arcade to see. "GAME OVER" and "INSERT COIN" are the arcade's;
   "PRESS START" the home consoles'.

   Alternatives:
     - the game's own variant, with this thin wrapper (ours): Richard
       Feldman's "Making Impossible States Impossible" (elm-conf 2016):
       a [Playing of game] can't exist without a game, a [Game_over]
       carries only its score;
     - a stack of scenes, a pause menu pushed over the game and popped
       back to it, the game unchanged below: the pushdown automaton of
       Robert Nystrom's Game Programming Patterns (2014), chapter
       "State"; our variant does it with [Paused of game];
     - scenes as separate apps, each with its view and update, the
       engine switching between them (Godot's and Unity's scenes, loaded
       from files, PICO-8's _init/_update/_draw swapped by hand): each
       scene is independent, but passing data between them (the score to
       the game over) needs a global or a message;
     - transitions (fading to black, wiping): nice, but fading a whole
       scene needs a group's alpha, which the renderers don't do yet
       (the TODO in Shape_render_software.ml); an exercise, as is an
       attract mode replaying recorded inputs (with the time-travel
       debugger of plan_teaching_other.md).

   Related work: Game Programming Patterns' "State" chapter; LÖVE's
   hump.gamestate (a stack, with enter/leave callbacks); Phaser's scenes;
   MakeCode Arcade's game.over(win) and its splash screens.
*)

open Playground

(* The game's scene, plus what this module keeps track of. [scene] is
 * the only field games set, through [go]; the others are read with the
 * functions below. *)
type 'scene t = {
  scene : 'scene;
  (* seconds since the scene started, and updates since then *)
  elapsed : number;
  frames : int;
  (* the time of the last [update], None before the first *)
  last : number option;
  (* the keyboard at the last update, and at the one before *)
  keys : keyboard;
  before : keyboard;
}

(* [start scene]: the scene the game begins with *)
val start : 'scene -> 'scene t

(* [update computer t]: to call first thing in the game's [update], every
 * frame: it counts the time, and remembers the keyboard. E.g. after
 * updates at the times 100.0, 100.5 and 101.25, elapsed = 1.25 (the
 * first update only notes the time) and frames = 3. *)
val update : computer -> 'scene t -> 'scene t

(* [go scene t]: switch to another scene, whose elapsed time and frames
 * start at 0; the keyboard is remembered, so a key held across the
 * switch isn't [pressed] in the new scene *)
val go : 'scene -> 'scene t -> 'scene t

(* [pressed key t]: whether [key] went down at the last update: down
 * now, up the frame before. E.g. holding space for 3 frames: pressed
 * at the first, not at the second or third. *)
val pressed : (keyboard -> bool) -> 'scene t -> bool

(* [blink period t shapes]: the [shapes] during the first half of each
 * [period] seconds of the scene, nothing during the second half; e.g.
 * with a period of 1., shown from 0 to 0.5, hidden from 0.5 to 1, shown
 * again from 1 ("PRESS SPACE", "INSERT COIN") *)
val blink : number -> 'scene t -> shape list -> shape list
