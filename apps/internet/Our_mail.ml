(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Our_mail.mli *)

let mailboxes =
  [ ("In", Our_mail_files.in_); ("Out", Our_mail_files.out); ("Trash", Our_mail_files.trash); ("Projects", Our_mail_files.projects) ]

let nicknames = Our_mail_files.nicknames
