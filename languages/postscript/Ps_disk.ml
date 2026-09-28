(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ps_disk.mli. Each program fits a screen of TinyPostScript's
   editor: twenty lines of at most fifty characters. *)

let tree =
  {|%!PS
% A tree: a trunk, then two smaller trees
% at its top, turned left and right -- a
% procedure calling itself, the stack for
% its arguments.
/tree {                   % len depth tree
  dup 0 gt {
    gsave
      1 index 0.08 mul setlinewidth
      exch                % depth len
      0 0 moveto 0 1 index lineto stroke
      0 1 index translate % to the top
      0.7 mul exch 1 sub  % shorter, less deep
      gsave 25 rotate 2 copy tree grestore
      -30 rotate tree
    grestore
  } { pop pop } ifelse
} def
306 80 translate 0.25 0.45 0.2 setrgbcolor
170 10 tree showpage
|}

let star =
  {|%!PS
% A star: a side, then a turn of 144
% degrees, five times. translate and rotate
% move the axes, not the drawing.
/side 250 def
/star {                   % x y star
  gsave
    translate 0 0 moveto
    5 { side 0 lineto
        side 0 translate
        -144 rotate } repeat
    closepath
    gsave 1 0.8 0.1 setrgbcolor fill grestore
    6 setlinewidth
    0.5 0.2 0 setrgbcolor stroke
  grestore
} def
181 480 star
/Helvetica findfont 36 scalefont setfont
250 200 moveto (A star) show showpage
|}

let rosette =
  {|%!PS
% Text is shapes too: the same word turned
% twelve times around a point, a shade of
% grey each time. for pushes 0, 1, ... 11.
/Helvetica findfont 28 scalefont setfont
306 420 translate
0 1 11 {
  /i exch def
  gsave
    i 30 mul rotate
    i 12 div setgray
    50 0 moveto (PostScript) show
  grestore
} for
showpage
|}

let pie =
  {|%!PS
% A pie chart from a table: arrays, get,
% aload, and arc.
/data [ 42 25 20 13 ] def
/colors [ [0.9 0.3 0.2] [0.2 0.5 0.9]
          [0.3 0.7 0.3] [0.95 0.7 0.1] ] def
306 420 translate
/angle 90 def
0 1 3 {
  /i exch def
  /sweep data i get 3.6 mul def
  colors i get aload pop setrgbcolor
  0 0 moveto
  0 0 220 angle angle sweep add arc
  closepath fill
  /angle angle sweep add def
} for
0 setgray 3 setlinewidth
0 0 220 0 360 arc stroke showpage
|}

let curve =
  {|%!PS
% A cubic Bezier curve: it leaves the first
% point toward the second, arrives at the
% fourth from the third, and passes through
% neither.
/p0 { 80 200 } def
/p1 { 160 650 } def
/p2 { 460 60 } def
/p3 { 540 500 } def
/dot { 8 0 360 arc fill } def   % x y dot
0.6 setgray 1 setlinewidth
p0 moveto p1 lineto p2 lineto p3 lineto
stroke
0 0 0.8 setrgbcolor 5 setlinewidth
p0 moveto p1 p2 p3 curveto stroke
0.8 0 0 setrgbcolor
p0 dot p1 dot p2 dot p3 dot
showpage
|}

let calculator =
  {|%!PS
% PostScript is a programming language
% first: the stack, and = to print (to the
% transcript, below the stack).
2 3 add 4 mul =           % (2 + 3) * 4
/fact { dup 1 le
  { pop 1 } { dup 1 sub fact mul }
  ifelse } def
10 fact =
/fib { dup 2 lt
  { } { dup 1 sub fib exch 2 sub fib add }
  ifelse } def
15 fib =
[ 1 2 3 ] { 2 mul } forall pstack
|}

let programs = [ ("tree", tree); ("star", star); ("rosette", rosette); ("pie", pie); ("curve", curve); ("calculator", calculator) ]
