(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Site.mli *)

let about (name : string) : (string * string) option =
  match name with
  | "home" -> Some (Site_pages.home, "text/html; charset=utf-8")
  | "history" -> Some (Site_pages.history, "text/html; charset=utf-8")
  | "form" | "form.html" -> Some (Site_pages.form, "text/html; charset=utf-8")
  | "netscape" -> Some (Site_pages.netscape, "text/html; charset=utf-8")
  | "css" -> Some (Site_pages.css, "text/html; charset=utf-8")
  | "picture.gif" -> Some (Site_pictures.picture_gif, "image/gif")
  | "picture.png" -> Some (Site_pictures.picture_png, "image/png")
  | "picture.jpg" -> Some (Site_pictures.picture_jpg, "image/jpeg")
  | _ -> None
