(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () = Testo.interpret_argv ~project_name:"web" (fun _env -> Unit_charset.tests @ Unit_entities.tests @ Unit_html_lexer.tests @ Unit_html_tree.tests @ Unit_line_mode.tests @ Unit_html_layout.tests @ Unit_table_layout.tests @ Unit_css.tests @ Unit_css_syntax.tests @ Unit_selectors.tests @ Unit_hit.tests @ Unit_forms.tests)
