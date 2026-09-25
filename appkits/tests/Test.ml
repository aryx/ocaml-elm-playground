(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () =
  Testo.interpret_argv ~project_name:"appkits" (fun _env -> List.concat [ Unit_document.tests; Unit_sheet.tests; Unit_typeset.tests; Unit_rich.tests; Unit_page.tests; Unit_paint.tests; Unit_embed.tests; Unit_slides.tests; Unit_hypertalk.tests; Unit_draw.tests; Unit_flow.tests; Unit_ics.tests; Unit_vcard.tests; Unit_tty.tests; Unit_basic.tests; Unit_browser.tests ])
