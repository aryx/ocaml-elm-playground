(* Code_anatomy: a program as a body, each of its systems a plate of the
   code map's X-ray (plan_codemap_v2.md, "Anatomy"; Map_v2):

     skeleton  the structure the rest hangs on: bones and joints, the
               configs' (Code_guide.skeleton)
     blood     the data carried round the skeleton: flowing along its
               joints, the model through update and view
     muscles   the code doing the work: a definition's size and its loops
     nerves    the signals: the keyboard, the mouse, subscriptions,
               commands, messages
     lungs     the exchange with the outside, where a program breathes:
               capabilities, files, sockets, the console, the platform
     skin      what a module shows the others: the names its .mli exposes

   The skeleton and the blood are judgement, written in the configs; the
   others are found in the code, by the words below (outside comments
   and strings, a word not in the middle of a name) and by measures: no
   judgement needed to see that a line reads the keyboard. The words are
   data, so that a config's layers can add their own later.

   Worked example (the tests'): in

     let update computer m = if computer.keyboard.space then fire m else m
     let save () = Out_channel.with_open_text "f" (fun oc -> ())

   the nerves are line 0 (computer.keyboard), the lungs line 1
   (Out_channel.). *)

type system = Skeleton | Blood | Muscles | Nerves | Lungs | Skin

val all : system list
val name : system -> string
val colour : system -> int * int * int

(* what it shows, in the code's words (the legend's) *)
val meaning : system -> string

(* the key toggling it in the X-ray: "1" to "6" *)
val key : system -> string

(* the systems shown, one setting for every map (as the glass's) *)
val shown : system list ref
val toggle : system -> unit

(* the words a system's lines contain *)
val nerve_words : string list
val lung_words : string list

(* what a file's anatomy is: the lines of its nerves and its lungs; its
   muscles, each top-level definition's lines (first, last) and strength,
   the density of its loops (a loop every fourth line makes 1; not
   size: the map's area already says it); its skin, the definitions' lines
   whose names [public] (its .mli's) exposes, [] if it has no .mli *)
(* claude: [hidden], the private definitions' extents: what the .mli
   does not show, shaded when the skin is on *)
type facts = { nerves : int list; lungs : int list; muscles : (int * int * float) list; skin : int list; hidden : (int * int) list }

val facts : Code_file.t -> public:string list option -> facts

(* the names an interface declares (its val, type, module, exception) *)
val public_names : Code_file.t -> string list
