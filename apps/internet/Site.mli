(* Site: the browsers' built-in site, the about: pages -- site/*.html
 * and its pictures, embedded by dune (Site_pages, Site_pictures), so
 * that TinyMosaic and TinyNetscape have something to show with no
 * network, and the golden frames something that never changes. The
 * same files served by tiny_httpd are the same site on a server. *)

(* about:NAME: its bytes and their Content-Type -- home, history,
 * form (or form.html, the home page's link being relative so as to be
 * right from tiny_httpd too), netscape (TinyNetscape's home: Netscape's
 * extensions to HTML), css (style sheets), and picture.gif, .png, .jpg *)
val about : string -> (string * string) option
