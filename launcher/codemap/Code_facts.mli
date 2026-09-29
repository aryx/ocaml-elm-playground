(* Code_facts: codellm's evidence (plan_codemap_v2.md, step 7): a
   directory's brief, in Markdown, for the LLM session writing its
   .codemapconfig (docs/claude_notes/codemapconfig_guidelines.md) -- what
   the analyses know, so that it reads the code with the right questions
   and writes anchors that hold:

   - the directory: its files and lines, what its parent's config says
     of it, whether it has a config and what the checker says of it;
   - each file: its lines and digest (what a config writes), its header
     comment (the author's words, the first after the copyright), its
     sections and tricks, its top-level definitions with their uses (in
     its file, from other files; Code_rank), the most used marked, and
     the files it uses and that use it, with how many uses; whether it
     is a program (a main).

   The judgement stays the LLM's: the brief never says what matters,
   only what is there. tinybox codemap -facts <root> <dir> prints it. *)

(* [brief ~guide ~sources ~dir]: the brief of [dir] (relative to the
   root of [sources], the paths and texts of every source, whose uses are
   counted over all of them), [guide] the configs as read *)
val brief : guide:Code_guide.t -> sources:(string * string) list -> dir:string -> string

(* a source's header comment: its first comment that is not a copyright,
   at most [max] lines *)
val header : ?max:int -> string -> string list
