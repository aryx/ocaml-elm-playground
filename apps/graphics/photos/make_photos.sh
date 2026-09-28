#!/bin/sh
# claude: TinyPhotoshop's photographs, made once from NASA's originals
# (works of the US government, in the public domain) as found on
# Wikimedia Commons, scaled to 400 by 400 and saved as baseline JPEGs,
# which our own reader (libs/graphics/images/jpeg) decodes. Rerun it
# only to remake them; it needs curl and ImageMagick.
#
# blue_marble.jpg: "The Blue Marble", AS17-148-22727, taken by the crew
#   of Apollo 17 on 7 December 1972, on the way to the Moon.
# aldrin.jpg: Buzz Aldrin on the Moon, AS11-40-5903, taken by Neil
#   Armstrong on 20 July 1969 (Apollo 11).
set -e
cd "$(dirname "$0")"
get () { curl -sSL -A "ocaml-elm-playground-fetch/1.0" -o "$1" "$2"; }
get original_blue_marble.jpg https://upload.wikimedia.org/wikipedia/commons/9/97/The_Earth_seen_from_Apollo_17.jpg
get original_aldrin.jpg https://upload.wikimedia.org/wikipedia/commons/9/98/Aldrin_Apollo_11_original.jpg
for p in blue_marble aldrin; do
  convert original_$p.jpg -resize 400x400^ -gravity center -extent 400x400 \
    -strip -interlace none -sampling-factor 2x2 -quality 85 $p.jpg
  rm original_$p.jpg
done
