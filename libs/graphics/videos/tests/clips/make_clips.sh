#!/bin/sh
# claude: how the MPEG-1 clips here were made, once (sh make_clips.sh,
# from this directory, with ffmpeg 6): our own clip (Our_media.clip, a
# ball and a square drawn by graphics/2d, dumped as Y4M by
# apps/media/tests/Dump_clip.ml) encoded by ffmpeg's MPEG-1 encoder --
# until graphics/videos/ has its own (plan_video_teaching.md, phase 5).
# A group of pictures of 12, two B frames between the references:
# I B B P B B P B B P B B, the classic structure.
set -e
root=$(cd ../../../../.. && pwd)
(cd "$root" && dune build ./apps/media/tests/Dump_clip.exe)
"$root/_build/default/apps/media/tests/Dump_clip.exe" > /tmp/ball_and_square.y4m
ffmpeg -v error -y -i /tmp/ball_and_square.y4m -c:v mpeg1video -q:v 4 -g 12 -bf 2 -f mpeg1video ball_and_square.m1v
# and what ffmpeg decodes from it: frames 0 (I), 3 (P) and 1 (B), for
# the tests to compare ours with
ffmpeg -v error -y -i ball_and_square.m1v -vf "select='eq(n\,0)+eq(n\,1)+eq(n\,3)'" -vsync 0 -pix_fmt yuv420p ball_and_square.expected.y4m
rm /tmp/ball_and_square.y4m
# and the clip with its sound (Our_media's AVI, dumped by
# apps/media/tests/Dump_media.ml) as an MPEG-1 system stream: the video
# as above, the blips as MP2, interleaved in packs of 2048 bytes
(cd "$root" && dune build ./apps/media/tests/Dump_media.exe)
tmp=$(mktemp -d)
"$root/_build/default/apps/media/tests/Dump_media.exe" "$tmp" > /dev/null
ffmpeg -v error -y -i "$tmp/ball_and_square.avi" -c:v mpeg1video -q:v 4 -g 12 -bf 2 -c:a mp2 -b:a 128k -f mpeg ball_and_square.mpg
rm -r "$tmp"
