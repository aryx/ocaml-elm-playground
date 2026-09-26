(* The environment of the block languages (TinyScratch, TinySnap): the
 * stage, the sprites under it with their scripts as text, the palette
 * of blocks by category, the scripts area where they are dragged, and
 * all that the mouse and the keys do there -- one program, laid out
 * and coloured by a [config], Scratch 2's or Snap!'s.
 *
 * The editing is appkits/blocks' (Block_layout, Block_edit), the
 * running libs/languages/scratch's (Scratch_run), the drawing
 * Scratch_look's. The stage steps every other frame (Scratch 2's 30 a
 * second), its clock counting the steps, so that a run is the same run
 * every time. *)

open Playground

type colors = {
  pane : color; (* the palette *)
  header : color; (* the categories, and the margins round the stage *)
  scripts : color;
  side : color; (* under the stage *)
  sheet : color; (* the text, the thumbnails *)
  line : color;
  ink : color; (* text on the panes *)
  sheet_ink : color;
  button_off : color;
}

type config = {
  title : string;
  menus : string;
  bar : color; (* the title bar *)
  theme : Scratch_look.theme;
  colors : colors;
  categories : Scratch_blocks.category list;
  (* the palette's blocks for a category, given the stage (its
     variables, its custom blocks) *)
  palette : Scratch_run.t -> Scratch_blocks.category -> Scratch_blocks.block list;
  make_block : bool; (* Snap!'s "Make a block" button, over Other's blocks *)
  stage : Scratch_look.frame;
  palette_x : float * float; (* left, right *)
  scripts_x : float * float;
  flag : float * float;
  stop : float * float;
  name_at : (float * float) option; (* the current sprite's name *)
}

type model

(* [app config stage ~current]: the project, the sprite whose
   scripts are shown; its flags: sprite=name (the sprite shown first),
   run=on (the green flag clicked) *)
val app : config -> Scratch_run.t -> current:string -> (model game, msg) app

(* the scripts of a text, in a column at the scripts area's top left *)
val column : config -> string -> Scratch_blocks.script list
