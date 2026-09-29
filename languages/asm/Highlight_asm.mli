(* Highlight_asm: an assembly file's tokens given their categories
   (Highlight_code's), and what it defines and uses, for the code map:
   the assemblers of an old Unix, as86's and gas's AT&T syntax (Linux
   0.01's boot.s and system_call.s), Plan 9's.

   Read line by line, without an assembler's grammar:

   - comments: /* ... */ over lines, and from | ! or # to the end of a
     line (as86's, gas's);
   - a statement is [label:] [mnemonic or .directive] [operands], several
     on a line with ; between them;
   - a label is a definition, a function to the map (Def_function); a
     name defined by = (SIG_CHLD = 17) a value; numeric labels (1:) and
     the registers are neither;
   - an operand's name is a use: of a label of the file (an occurrence
     bound to it), else of a name defined elsewhere (a reference).

   a.out's convention is that C's names have a leading underscore in
   assembly (C's main is _main): a definition's and a reference's names
   lose it, so that C's system_call and assembly's _system_call are one
   name to the map (Code_names treats .s files as C's, one namespace).

   Worked example (the tests'):

     _system_call:            system_call: Def_function, defined
         call _sys_call_table(,%eax,4)   sys_call_table: a reference
         jmp ret_from_sys_call           bound to the file's label *)

val analyze : string -> Highlight_code.analysis
