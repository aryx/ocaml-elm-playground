(* PuzzleScript: a game as a map and a few rules, on top of the Playground.

   The whole of Sokoban, the game, is this:

     rule "> @ | $  ->  > @ | > $"        and   win = all_on '$' '.'

   which reads: wherever a hero moving some way has a crate in front of
   it, the crate moves that way too. There is no update function in a
   game written here, and no view: you give the things, a map of them,
   the rules, and what winning means, and this module plays it. It is
   the third of the playground's borrowed ways of programming, beside
   Logo.mli's turtle and Bigbang.mli's world programs.

   {b Where it comes from}

   There is a line of systems, most of them from people trying to teach
   programming, whose idea is that a game is a grid and a handful of
   before/after pictures:

     - Seymour Papert's Logo (1967) is the root of all of this and of
       Logo.mli: a child's program is a few commands, and the machine
       is something you think with rather than about ({i Mindstorms},
       1980);
     - KidSim, later Cocoa, later Stagecast Creator (David Canfield
       Smith, Allen Cypher and Kurt Schmucker, Apple, 1994): children
       make games by {i showing} the computer a rule -- here is the
       board before, here it is after -- and never write a line. The
       rewrite rule as a teaching tool starts here;
     - AgentSheets (Alexander Repenning, Colorado, 1991 on) and later
       AgentCubes: grids of agents, rules given the same way, and
       twenty years of papers on what children actually manage with
       them ("computational thinking patterns");
     - PuzzleScript (Stephen Lavelle, "increpare", 2013): the same idea
       from the other side, an indie game designer's tool, in a browser,
       where a game is a text file -- objects, a legend, layers, a map,
       rules -- and thousands of small games were made in it. This
       module is a small PuzzleScript;
     - TileCode (Thomas Ball, Stefania Druga et al., Microsoft Research,
       2020, see Tilemap.mli's related work): rules as 3x3 pictures
       around a sprite, made on the handheld itself, and a claim worth
       testing here -- that ten to fifteen rules are enough for a
       variety of games;
     - and, off to the side, the Video Game Description Language (Tom
       Schaul, 2013), the same compression for a different reason: a
       short description per game, so that one program can be made to
       play hundreds of them.

   Next to Bigbang.mli's How to Design Programs (Felleisen and others),
   that is two traditions in this directory: HtDP hands you the shape of
   a program and asks you to fill it in; this one takes the program away
   and leaves you the rules. (Names and dates from memory, to check.)

   {b Things, and why they live on layers}

   Without layers a cell of the map is one character, so a crate
   standing on a target needs a character of its own, and so does the
   hero on a target, and every rule about crates has to be written
   twice. That is how a hand-written Sokoban grows (see
   TinySokoban.ml, which does it by hand). Here a cell holds one
   thing per layer:

        layer 1    . . @ $ .      what moves: hero, crates, walls
        layer 0    . * . * .      what is painted on the floor: targets

   and "a crate on a target" is not a new thing, it is two things in one
   cell. Layers are also the only collision rule there is: a thing can
   move into a cell if that cell has room {i on its own layer}. Put the
   walls and the crates on the same layer and a crate cannot be pushed
   into a wall, for free.

   {b A turn}

   1. the key is pressed: every hero is marked "moving that way";
   2. the rules run, again and again, until the board stops changing --
      they mark other things as moving, and make things appear and
      disappear;
   3. everything still marked moves one cell, if its layer has room
      there; the board is swept again and again, because the cell one
      thing leaves may be the one another was waiting for:

        > @ > $ .      the crate has room and the hero has not, so the
        . @ > $ .      crate goes first and the hero follows on the
        . . @ $ .      next sweep -- a push, out of one rule and a loop;

   4. the rules made [~late:true] run, on the board as it now is (this
      is where gravity and tidying up go);
   5. the marks are dropped, and the win conditions are read.

   {b Writing a rule}

   A rule is one string, two sides around [->], each side a row of cells
   separated by [|], each cell a list of things:

     "> @ | $  ->  > @ | > $"

   A thing is its character. Before it may come one of:

     [>]           moving the way the rule is being read
     [<]           moving the other way
     [moving]      moving at all
     [stationary]  not moving
     [no]          not here

   The rule is read in all four directions unless [?dirs] says
   otherwise, so the one rule above pushes in all four. A thing named on
   the left and not on the right, on the same layer, is taken away: that
   is how a rule eats something.

   {b What this leaves out} of the real PuzzleScript: objects have
   names and a legend of words (here a thing {i is} its character),
   sprites have up to ten colours, and there are sounds, [again] (run
   the turn again, for things that keep falling), [startloop], random
   rules, rigid bodies, and an editor that plays the game as you type.
   What is here is the part that makes a game.

   Used by examples/PuzzleScriptSokoban.ml and
   examples/PuzzleScriptBoulders.ml -- two games out of the same engine,
   which is the point of the thing. *)

open Playground

(*****************************************************************************)
(* The four directions *)
(*****************************************************************************)

type dir = Up | Down | Left | Right

(* for [rule ~dirs]: the four, the two sideways, the two up and down *)
val every : dir list
val horizontal : dir list
val vertical : dir list

(*****************************************************************************)
(* Things *)
(*****************************************************************************)

(* a thing in the world: the character that stands for it in a map and
   in the rules, the colour it is drawn in, and the layer it lives on
   (see the header: things that must not share a cell go on one layer) *)
type thing = { glyph : char; color : color; layer : int; art : string list }

(* [thing '@' blue ~layer:1]: a thing drawn as a square of its colour.
   With [~art], it is drawn as that little picture instead, one
   character per pixel, '.' for nothing and anything else for the
   thing's colour -- PuzzleScript's 5 x 5 sprites, in one colour. *)
val thing : ?art:string list -> char -> color -> layer:int -> thing

(*****************************************************************************)
(* Rules *)
(*****************************************************************************)

type rule

(* [rule "> @ | $ -> > @ | > $"]: see "Writing a rule" in the header.
   [?dirs] are the directions it may be read in (all four by default),
   [?late:true] runs it after things have moved. Raises [Failure] if the
   two sides don't have the same number of cells, or a word is neither a
   thing's character nor one of the prefixes. *)
val rule : ?dirs:dir list -> ?late:bool -> string -> rule

(*****************************************************************************)
(* Winning *)
(*****************************************************************************)

type win

(* [all_on '$' '.']: every crate is on a target (and there is at least
   one crate) -- PuzzleScript's "All Crate on Target" *)
val all_on : char -> char -> win

val some_on : char -> char -> win
val no_on : char -> char -> win

(* [none_left 'D']: there are no diamonds left on the board *)
val none_left : char -> win

(*****************************************************************************)
(* A game *)
(*****************************************************************************)

type t

(* [make ~things ~player ~rules ~wins levels]: a whole game. [player] is
   the character of the thing the keys move. [?legend] gives a map
   character that stands for several things at once, e.g. ('*', "$.") for
   a crate already on a target: a map character not in the legend is the
   thing of that character, or nothing at all. The level is won when
   every one of [wins] holds; a game with no [wins] is never won. *)
val make :
  things:thing list ->
  player:char ->
  ?legend:(char * string) list ->
  rules:rule list ->
  wins:win list ->
  ?background:color ->
  string list list ->
  t

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type state

(* the app to hand to Playground_platform.run_app: arrows (or wasd)
   move, z undoes, r restarts, space goes on to the next level *)
val play : t -> (state Playground.game, msg) app

(*****************************************************************************)
(* Playing it without a screen *)
(*****************************************************************************)

(* the board of a level, for tests and for solvers *)
type board

val board : t -> int -> board

(* [turn game (Some Right) b]: the board after one turn (the five steps
   of the header). Boards are values: the old one is untouched, which is
   what makes undo a list and a solver a search. *)
val turn : t -> dir option -> board -> board

val won : t -> board -> bool

(* the things in a cell, from the lowest layer up; [] outside the board *)
val at : board -> int -> int -> char list

(* the board back as rows of characters, the legend used wherever a cell
   holds exactly what one of its entries spells *)
val to_strings : t -> board -> string list
