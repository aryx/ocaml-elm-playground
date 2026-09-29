(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* claude: Highlight_asm.mli's worked example *)

let src = "| a comment\n_system_call:\n\tcall _sys_call_table(,%eax,4)\n\tjmp ret_from_sys_call\nret_from_sys_call:\n\tiret\n"

let tests =
  [
    Testo.create "asm: definitions and references" (fun () ->
        let a = Highlight_asm.analyze src in
        let names = List.map (fun (d : Highlight_code.definition) -> d.dname) a.definitions in
        Alcotest.(check (list string)) "labels, a.out's _ dropped" [ "system_call"; "ret_from_sys_call" ] names;
        let refs = List.map (fun (r : Highlight_code.reference) -> r.rname) a.references in
        Alcotest.(check (list string)) "the names defined elsewhere" [ "sys_call_table" ] refs;
        Alcotest.(check int) "two labels and a use of one, bound" 3 (List.length a.occurrences));
  ]

let () = Testo.interpret_argv ~project_name:"asm" (fun _ -> tests)
